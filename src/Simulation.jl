import Base.deepcopy

export construct_model
export new_simulation, finish_simulation!
export finish_init!
export apply_transition, apply_transition!
export param
export aggregate
export copy_simulation

# this is mutable, as we assign write -> read in finish_write!
mutable struct AgentReadWrite{T}
    state::Vector{T}
    # This is a Vector of rows (AgentNr) that can be reused for the
    # next agents. It is redudant to died: died[e in reuseable] == true
    # Also this field is for immortal agents always just an empty vector
    reuseable::Vector{AgentNr}
    # This field is for immortal agents always just an empty vector
    died::Vector{Bool}
end

AgentReadWrite(T::DataType) =
    AgentReadWrite{T}(Vector{T}(), Vector{AgentNr}(), Vector{Bool}())

mutable struct MPIWindows
    # Access via shared memory
    shmstate::Union{MPI.Win, Nothing}
    shmdied::Union{MPI.Win, Nothing}
    # we are between prepare_read! and finished_mpi!
    prepared::Bool
end

MPIWindows() = MPIWindows(nothing, nothing, false)

mutable struct AgentFields{T}
    read::AgentReadWrite{T}
    write::AgentReadWrite{T}
    # the state of all agents on the same node
    shmstate::Vector{Vector{T}}
    # the died flag of all agents on the same node
    shmdied::Vector{Vector{Bool}}
    # the state of agents on others nodes
    foreignstate::Dict{AgentID, T}
    # the died flag of agents on other nodes
    foreigndied::Dict{AgentID, Bool}
    
    nextid::AgentNr
    mpiwindows::MPIWindows
    # the last time (in number of transitions called) that the agentstate
    # was send due to the evaluation of an specific edgetype 
    last_transmit::Dict{DataType, Int64}
    # the last time that the agenttype was writable
    last_change::Int64
end

AgentFields(T::DataType) =
    AgentFields(AgentReadWrite(T), # read
                AgentReadWrite(T), # write
                [ Vector{T}() for _ in 1:mpi.shmsize ], #shmstate
                [ Vector{Bool}() for _ in 1:mpi.shmsize ], #shmdied
                Dict{AgentID, T}(), # foreignstate
                Dict{AgentID, Bool}(), # foreigndied
                AgentNr(1), #nextid
                MPIWindows(), 
                Dict{DataType, Int64}(), #last_transmit
                0) #last_change

mutable struct EdgeFields{ET, EST}
    read::ET
    write::ET
    # when a edge is added with target-agents on a different process
    # it is stored here
    storage::EST
    # this tracks edges with source-agents on a different node, the
    # outer vector is the agenttype nr, the middle vector the process
    # index. It's possible that the sets are containing agents that
    # are died in the meantime, as edges/fromids can not be removed
    # from the sets, only the complete sets are resetted when the
    # edgetype is rewritten.
    accessible::Vector{Vector{Set{AgentID}}}
    # the last time (in number of transitions called) that the storage
    # was send to the other processes (via edges_alltoall!)
    last_transmit::Int64
    # the last time that the edgetype was writable
    last_change::Int64
    # is set to true, when this field is in the read vector of a
    # transition function
    readable::Bool
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
                          $(edgestorage_type(T, typeinfos.edges_attr[T])()),
                          [ [ Set{AgentID}() for _ in 1:mpi.size ] for _ in 1:MAX_TYPES ],
                          0,0,false)))
        for T in typeinfos.edges_types ] 

    
    nodefields = [
        Expr(Symbol("="),
             :($(Symbol(T))::AgentFields{$T}),
             :(AgentFields($T)))
        for T in typeinfos.nodes_types ]

    fields = Expr(:block,
                  :(model::Model),
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
        construct_edge_methods(T, typeinfos, simsymbol)
    end

    # Construct all type specific functions for the agent typeinfos
    for T in typeinfos.nodes_types
        construct_agent_methods(T, typeinfos, simsymbol)
    end

    construct_prettyprinting_methods(simsymbol)

    Model(typeinfos, name)
end

num_agenttypes(sim) = length(sim.typeinfos.nodes_types)


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
        model = $model,
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
    foreach(1:l) do idx
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
the rank on which the agent "lives" after `finish_init!`.

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
[`apply_transition!`](@ref) and [`finish_simulation!`](@ref)
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
            elseif partition_algo == :EqualAgentNumbers
                @info "Partitioning the Simulation / equal number of nodes per type"
                for T in sim.typeinfos.nodes_types
                    ids = map(i -> agent_id(sim, AgentNr(i), T), keys(readstate(sim, T)))
                    _create_equal_partition(partition, ids)
                end
            else
                @error "the partition_algo given is unknown"
            end
        end
        if mpi.isroot
            @info "Distributing the Simulation"
        end
        distribute!(sim, partition)
    else
        if return_idmapping
            idmapping = Dict{AgentID, AgentID}()
            for T in sim.typeinfos.nodes_types
                for id in keys(readstate(sim, T))
                    aid = agent_id(sim, AgentNr(id), T)
                    idmapping[aid] = aid
                end
            end
            idmapping
        else
            nothing
        end
    end

    foreach(finish_write!(sim), sim.typeinfos.nodes_types)
    foreach(finish_write!(sim), sim.typeinfos.edges_types)

    sim.initialized = true

    sim.num_transitions = 1

    return_idmapping ? idmapping : nothing
end 

# this function is not exported and should be only used for unit tests
function updateids(idmap, oldids)
    map(oldids) do id
        idmap[Vahana.remove_process(id)]
    end
end


"""
    copy_simulation!(sim)

Create an independent copy of the simulation `sim`. Since part of the
memory is allocated via MPI, you should not use deepcopy.
"""
function copy_simulation(sim)
    copy = deepcopy(sim)
    # we copied also the MPI Windows, so that the copy points to the same
    # memory as the original. So this must be fixed.
    foreach(T -> copy_shm!(copy, T), sim.typeinfos.nodes_types)
    copy
end


"""
    finish_simulation!(sim)

Remove all agents/edges and rasters from the simulation to minimize
the memory footprint. The `parameters` and `globals` of the simulation
are still available, the remaining state of the simulation is
undefined.
"""
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
        init_field!(sim, T)
    end       
    for T in sim.typeinfos.edges_types
        empty!(edgeread(sim, T))
        empty!(edgewrite(sim, T))
    end
    empty!(sim.rasters)
    nothing
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
    apply_transition!(sim, func, call, read, write; 
                      add_existing = Vector{DataType}())

Apply the transition function `func` to the simulation state. 

`call` is a vector of agent types, `read` and `write` are vectors of
agent and/or edge types. 

`call` determines for which agent types the transition function `func`
is called. Within the transition function, an agent has access to the
state of agents (including its own state) and to edges only if
their types are specified in the `read` vector. Accordingly, the agent
can change its own state and/or create new agents or edges only if
their types are specified in the `write` vector.

Assume that T is an agent type that is in `call` state. In case T is
also in `read`, the transition function must have the following
signature: `function(agent::T, id::AgentID, sim::Simulation)`. If T is
not in `read`, it must have the signature `function(::Val{T},
id::AgentID, sim::Simulation)`. 

If T is in `write`, the transition function must return either an agent
of type T or `nothing`. If `nothing` is returned, the agent will be
removed from the simulation, otherwise the agent with id `id`
(starting with the next `apply_transition!` call) will have the
returned state. 

TODO DOC add_existing

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
    
    # before we call prepare_write! we must ensure that all edges
    # in the storage that are accessible are distributed to the correct
    # ranks. Also we must first transfer the edges, as they determine,
    # which agentstate is available, and the agentstate of other nodes
    # are transmitted in prepare_read! of an agenttype
    readableET = filter(w -> w in sim.typeinfos.edges_types, read)
    foreach(T -> prepare_read!(sim, read, T), readableET)
    readableAT = filter(w -> w in sim.typeinfos.nodes_types, read)
    foreach(T -> prepare_read!(sim, read, T), readableAT)
    
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

    # must be incremented after the transition, so that read only
    # functions like aggregate tries to transfer the necessary states
    # only once 
    sim.num_transitions = sim.num_transitions + 1
    
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

Call [`apply_transition!`](@ref) with a copy of the simulation so that the
state of `sim` itself is not changed.

Can be very useful during development, especially if Vahana is used in
the REPL. However, for performance reasons, this function should not
be used in the final code.

Returns the copy of the simulation.

See also [`apply_transition!`](@ref)
"""
function apply_transition(sim,
                   func::Function,
                   call::Vector,
                   read::Vector,
                   write::Vector;
                   kwargs ...) 
    newsim = deepcopy(sim)
    apply_transition!(newsim, func, call, read, write; kwargs ...)
    newsim
end

function apply_transition(func::Function,
                   sim,
                   call::Vector,
                   read::Vector,
                   write::Vector;
                   kwargs ...)
    apply_transition(sim, func, call, read, write; kwargs ...)
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
