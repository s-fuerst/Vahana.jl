export construct_model
export new_simulation, finish_simulation!
export finish_init!
export apply_transition, apply_transition!
export param
export aggregate

# this is mutable, as we assign write -> read in finish_write!
mutable struct AgentReadWrite{T}
    state::Vector{T}
    # This is a Vector of rows (AgentNr) that can be reused for the
    # next agents. It is redudant to died: died[e in reuseable] == true
    # Also this field is for immortal agents always just an empty vector
    reuseable::Vector{AgentNr}
    # This field is for immortal agents always just an empty vector
    died::Vector{Bool}
    # The last transition when prepare_read! (oder prepare_write!) was called
    last_change::Int64
end

AgentReadWrite(T::DataType) =
    AgentReadWrite{T}(Vector{T}(), Vector{AgentNr}(), Vector{Bool}(), 0)

mutable struct MPIWindows
    # Access via shared memory
    shmstate::Union{MPI.Win, Nothing}
    shmdied::Union{MPI.Win, Nothing}
    # Access to other nodes via MPI.Get
    nodestate::Union{MPI.Win, Nothing}
    nodedied::Union{MPI.Win, Nothing}
    # we are between prepare_read! and finished_mpi!
    prepared::Bool
end

MPIWindows() = MPIWindows(nothing, nothing, nothing, nothing, false)

mutable struct AgentFields{T}
    read::AgentReadWrite{T}
    write::AgentReadWrite{T}
    # the state of all agents on the same node
    shmstate::Vector{Vector{T}}
    # the died flag of all agents on the same node
    shmdied::Vector{Vector{Bool}}
    reuse::Vector{Reuse}
    nextid::AgentNr
    mpiwindows::MPIWindows
end

AgentFields(T::DataType) =
    AgentFields(AgentReadWrite(T),
                AgentReadWrite(T),
                [ Vector{T}() for _ in 1:mpi.shmsize ],
                [ Vector{Bool}() for _ in 1:mpi.shmsize ],
                Vector{Reuse}(),
                AgentNr(1),
                MPIWindows())

mutable struct EdgeFields{ET, EST}
    read::ET
    write::ET
    storage::EST
    # the last time (in number of transitions called) that the storage
    # was send to the other processes (via edges_alltoall!)
    last_transmit::Int64
    # the last time that the edgetype was writable
    last_change::Int64
end

"""
    construct_model(types::ModelTypes, name::String)

Adds a structure and methods corresponding to the type information of
`types` to the Julia session. The new structure is named `name`, and
all methods are specific to this structure using Julia's multiple
dispatch concept, so it is possible to have different models in the
same Julia session (as long as `name` is different).

Returns a [`Model`](@ref) that can be used in [`new_simulation`](@ref) to create 
a concrete simulation.
"""
function construct_model(typeinfos::ModelTypes, name::String) 
    simsymbol = Symbol(name)

    edgefields = [
        Expr(Symbol("="),
             :($(Symbol(T))::EdgeFields{$(edgefield_type(T, typeinfos.edges_attr[T])),
                                        $(edgestorage_type(T, typeinfos.edges_attr[T]))}),
             :(EdgeFields($(edgefield_constructor(T, typeinfos.edges_attr[T])),
                          $(edgefield_constructor(T, typeinfos.edges_attr[T])),
                          $(edgestorage_constructor(T, typeinfos.edges_attr[T])),
                          0,0)))
        for T in typeinfos.edges_types ] 

    
    nodefields = [
        Expr(Symbol("="),
             :($(Symbol(T))::AgentFields{$T}),
             :(AgentFields($T)))
        for T in typeinfos.nodes_types ]

    fields = Expr(:block,
                  :(modelname::String),
                  :(name::String),
                  :(params::P),
                  :(globals::G),
                  :(typeinfos::ModelTypes),
                  :(rasters::Dict{Symbol, Array}),
                  :(initialized::Bool),
                  :(intransition::Bool),
                  :(num_transitions::Int64),
                  edgefields...,
                  nodefields...)
    
    # the true in the second arg makes the struct mutable
    strukt = Expr(:struct, true, :($simsymbol{P, G}), fields)

    kwdefqn = QuoteNode(Symbol("@kwdef"))
    # nothing in third argument is for the expected LineNumberNode
    # see also https://github.com/JuliaLang/julia/issues/43976
    Expr(:macrocall, Expr(Symbol("."), :Base, kwdefqn), nothing, strukt) |> eval

    # Construct all type specific functions for the edge typeinfos
    for T in typeinfos.edges_types
        construct_edge_methods(T, typeinfos.edges_attr[T], simsymbol)
    end

    # Construct all type specific functions for the agent typeinfos
    for T in typeinfos.nodes_types
        construct_agent_methods(T, typeinfos, simsymbol)
    end

    construct_prettyprinting_methods(simsymbol)

    Model(typeinfos, name)
end


"""
    new_simulation(model::Model, params = nothing, globals = nothing; name)

Creates and return a new simulation object, which stores the complete state 
of a simulation. 

`model` is an `Model` instance created by [`construct_model`](@ref).

`params` must be a struct (or `nothing`) that contains all parameters of a
simulation. Parameter values are constant in a simulation run and can be
retrieved via the [`param`](@ref) function.

`globals` must be a mutable struct (or `nothing`). The values of these fields are
accessible for all agents via the [`getglobal`](@ref) function. The values can
be changed by calling [`setglobal!`](@ref) or [`pushglobal!`](@ref). 

The optional keyword argument `name` is used as meta-information about
the simulation and has no effect on the dynamics, since `name` is not
accessible in the transition functions. If `name` is not given, the
name of the model is used instead.

The simulation starts in an uninitialized state. After adding the
agents and edges for the initial state, it is necessary to call
[`finish_init!`](@ref) before applying a transition function for the first
time.

See also [`construct_model`](@ref), [`param`](@ref),
[`getglobal`](@ref), [`setglobal!`](@ref), [`pushglobal!`](@ref)
and [`finish_init!`](@ref)
"""
function new_simulation(model::Model,
                 params::P = nothing,
                 globals::G = nothing;
                 name = model.name) where {P, G}
    sim = @eval $(Symbol(model.name))(
        modelname = $(model.name),
        name = $name,
        params = $params,
        globals = $globals,
        typeinfos = $(model.types),
        rasters = Dict{Symbol, Array}(),
        initialized = false,
        intransition = false,
        num_transitions = 0,
    )

    for T in sim.typeinfos.edges_types
        init_field!(sim, T)
        init_storage!(sim, T)
    end

    for T in sim.typeinfos.nodes_types
        init_field!(sim, T)
    end

    global show_second_edge_warning = true
    
    sim
end

# pipeable versions 
construct_model(name::String) = types -> construct_model(types, name)

new_simulation(params::P = nothing,
               globals::G = nothing;
               kwargs...) where {P, G} =
                   model -> new_simulation(model, params, globals; kwargs...)

function _create_equal_partition(d, ids)
    l = length(ids)
    ids_per_rank = Int64(ceil(l / mpi.size)) + 1
    ranks = foreach(1:l) do idx
        d[ids[idx]] = div(idx, ids_per_rank) + 1
    end
end

"""
    finish_init!(sim::Simulation; distribute::Bool, 
                 partition::Dict{AgentID, ProcessID})

Finish the initialization phase of the simulation. 

`partition` is an option keyword and allows to specify an assignment
of the agents to the individual MPI ranks. The dictonary must contain
all agentids created on the rank as key, the corresponding value is
the rank on which the agent "lives" after `finish_init`.

`finish_init!` must be called before applying a transition function. 

!!! info 

    When a simulation is run on multiple PEs, per default the graph
    found on rank 0 will be partitioned using Metis, and distributed
    to the different ranks. Which means that it's allowed to run the
    initialization phase on all ranks (there is no need for a
    mpi.isroot check), but then all added agents and edges on other
    ranks then 0 will be discarded. If this is not intended
    `distribute` must be set to false.


See also [`register_agenttype!`](@ref), [`register_edgetype!`](@ref),
[`apply_transition!`](@ref) 
"""
function finish_init!(sim;
               partition = Dict{AgentID, ProcessID}(),
               return_idmapping = false, partition_algo = :Metis, distribute = true)
    @assert ! sim.initialized "You can not call finish_init! twice for the same simulation"
    
    foreach(finish_write!(sim), keys(sim.typeinfos.nodes_type2id))
    foreach(finish_write!(sim), sim.typeinfos.edges_types)

    if ! distribute
        sim.initialized = true
        return nothing
    end

    if mpi.rank > 0
        partition = Dict{AgentID, ProcessID}()
    end
    
    idmapping = if mpi.size > 1
        # we are creating an own partition only when no partition is given
        if length(partition) == 0 && mpi.isroot
            if partition_algo == :Metis
                @info "Partitioning the Simulation with Metis"
                vsg = vahanasimplegraph(sim; show_ignorefrom_warning = false)
                part = Metis.partition(vsg, mpi.size; alg = :RECURSIVE)
                for (i, p) in enumerate(part)
                    partition[vsg.g2v[i]] = p
                end
            else
                @info "Partitioning the Simulation / equal number of nodes per type"
                for T in sim.typeinfos.nodes_types
                    ids = map(i -> agent_id(sim, AgentNr(i), T), keys(readstate(sim, T)))
                    _create_equal_partition(partition, ids)
                end
            end
        end
        if mpi.isroot
            @info "Distributing the Simulation"
        end
        distribute!(sim, partition)
    else
        idmapping = Dict{AgentID, AgentID}()
        for T in sim.typeinfos.nodes_types
            for id in keys(readstate(sim, T))
                aid = agent_id(sim, AgentNr(id), T)
                idmapping[remove_reuse(aid)] = aid
            end
        end
        idmapping
    end

    # TODO: There is already a finiwh_write! in distribute!, maybe we can remove this?
    foreach(finish_write!(sim), keys(sim.typeinfos.nodes_type2id))
    foreach(finish_write!(sim), sim.typeinfos.edges_types)

    sim.initialized = true

    return_idmapping ? idmapping : nothing
end 

# this function is not exported and should be only used for unit tests
function updateids(idmap, oldids)
    map(oldids) do id
        idmap[Vahana.remove_process(Vahana.remove_reuse(id))]
    end
end



# TODO SHM write finish_simulation! . free shm stuff, remove references.
# sim should be after finish_simulation! in the same state as new_simulation.
function finish_simulation!(sim)
    for T in sim.typeinfos.nodes_types
        shmstatewin = getproperty(sim, Symbol(T)).mpiwindows.shmstate
        if ! isnothing(shmstatewin)
            MPI.free(shmstatewin)
        end
        shmdiedwin = getproperty(sim, Symbol(T)).mpiwindows.shmdied
        if ! isnothing(shmdiedwin)
            MPI.free(shmdiedwin)
        end
        # TODO release all references
    end       
end
######################################## Transition


"""
    param(sim::Simulation, name)

Returns the value of the field `name` of the `params` struct from the
Simulation constructor.

See also [`construct_model`](@ref)
"""
param(sim, name) = getfield(sim.params, name)

function maybeadd(coll,
           id,
           agent) 
    # the coll[id] writes into another container then
    # we use for the iteration
    @inbounds coll[id] = agent
    nothing
end

function maybeadd(_,
           ::AgentNr,
           ::Nothing)
    nothing
end

prepare_write!(sim, add_existing) = t -> prepare_write!(sim, t in add_existing, t)

finish_write!(sim) = t -> finish_write!(sim, t)


"""
    apply_transition!(sim, func, compute, accessible, rebuild; 
                      invariant_compute = false, add_existing = Vector{DataType}())

Apply the transition function `func` to the simulation state. 

A transition function must have the following signature: `function(agent::T,
id::AgentID, sim::Simulation)` and must return either an agent of type
T or `nothing`. If `nothing` is returned, the agent will be removed
from the simulation, otherwise the agent with id `id` will have
(starting with the next `apply_transition!` call) the returned state.
In the case that all agents do not change their state and only modify
the state of the simulation by adding edges or new agents, the
keyword argument `invariant_compute` can be set to true. In this case the
returned value of the transition function will be ignored.

`compute` is a vector of Agent types. `func` is called for every agent
with a type that is listed in `compute`, so a method for each of this types
must be implemented.

`accessible` is a vector of Agent and/or Edge types. This vector must
list all types that are accessed directly (e.g. via
[`agentstate`](@ref) or indirectly (e.g. via [`neighborstates`](@ref)
in the transition function.

`rebuild` is a vector of Agent and/or Edge types. All the instances of
agents or edges with a type in (`rebuild` - `add_existing`) will be
removed and must be added again inside of `func`.
[`add_agent!`](@ref), [`add_agents!`](@ref), [`add_edge!`](@ref) and
[`add_edges!`](@ref) can be only called for types in `rebuild` or
`compute`. In the case, that a transition function should only add additional
edges for some types, these types must be listed in the `add_existing`
vector.

TODO DOC: add_existing

TODO: in accessible currently only the agent types are needed? Do we need
also the edgetypes? If yes, check that in the edge_functions

TODO: rebuild and add_existing is a confusing combination, rebuild = write
would be nicer?

See also [`apply_transition`](@ref)
"""
function apply_transition!(sim,
                    func::Function,
                    call::Vector,
                    read::Vector,
                    write::Vector;
                    add_existing = Vector{DataType}())
    @assert sim.initialized "You must call finish_init! before apply_transition!"

    # must be set to true before prepare_read! (as this calls add_edge!)
    sim.intransition = true
    sim.num_transitions = sim.num_transitions + 1
    
    # before we call prepare_write! we must ensure that all edges
    # in the storage that are accessible are distributed to the correct
    # ranks.
    foreach(T -> prepare_read!(sim, T), read)
    
    foreach(prepare_write!(sim, [call; add_existing]), write)

    MPI.Barrier(MPI.COMM_WORLD)
    
    for C in call
        rfunc = C in read ? transition_with_read! : transition_without_read!
        wfunc = C in write ? transition_with_write! : transition_without_write!
        rfunc(wfunc, sim, func, C)
    end
    
    MPI.Barrier(MPI.COMM_WORLD)

    foreach(T -> finish_read!(sim, T), read)

    # we must first call finish_write! for the agents, as this will
    # remove the edges where are died agents are involved (and modifies
    # EdgeType_write).
    writeableAT = filter(w -> w in sim.typeinfos.nodes_types, write)
    foreach(finish_write!(sim), writeableAT)

    writeableET = filter(w -> w in sim.typeinfos.edges_types, write)
    foreach(finish_write!(sim), writeableET)

    sim.intransition = false
    
    sim
end

function apply_transition!(func::Function,
                    sim,
                    call::Vector,
                    read::Vector,
                    write::Vector;
                    kwargs ...)
    apply_transition!(sim, func, call, read, write; kwargs ...)
end


"""
    apply_transition(sim, func, compute, networks, rebuild) -> Simulation

Wraps [`apply_transition!`](@ref) with a deepcopy so that the state of
`sim` itself is not changed.

Can be very useful during development, especially if Vahana is used in
the REPL. However, for performance reasons, this function should not
be used in the final code.

See also [`apply_transition!`](@ref)
"""
function apply_transition(sim,
                   func,
                   compute::Vector,
                   networks::Vector,
                   rebuild::Vector;
                   kwargs ...) 
    newsim = deepcopy(sim)
    apply_transition!(newsim, func, compute, networks, rebuild; kwargs ...)
    newsim
end

######################################## aggregate

"""
    aggregate(sim, f, op, ::Type{T}; kwargs ...)

Calculate an aggregated value, based on the state of all agents or
edges of type T.

`f` is applied to all of these agents or edges and then `op` is used to
reduce (aggregate) the values returned by `f`.

aggregate is based on mapreduce, `f`, `op` and `kwargs` are
passed directly to mapreduce, while `sim` and `T` are used to determine the
iterator.

TODO DOC: additional kwargs
"""
function aggregate(::__MODEL__, f, op, ::Type; kwargs...) end
