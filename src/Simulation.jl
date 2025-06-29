import Base.deepcopy
import Logging.with_logger
import Graphs.nv
using DataFrames
export create_model
export create_simulation, finish_simulation!, copy_simulation
export finish_init!
export apply, apply!
export param, set_param!
export mapreduce

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
    # store which edges must be removed on other processes,
    # first UInt64 is the source, second UInt64 the target
    # the source is set to 0 if all edges of the target should be removed.
    # the vector index is the rank+1 of the target agent.
    removeedges::Vector{Vector{Tuple{AgentID, AgentID}}}
    # this tracks edges with source-agents on a different node, the
    # outer vector is the agenttype nr, the middle vector the process
    # index. It's possible that the sets are containing agents that
    # are died in the meantime, as edges/fromids can not be removed
    # from the sets, only the complete sets are resetted when the
    # edgetype is rewritten.
    accessible::Vector{Vector{Set{AgentID}}}
    # the last time (in number of transitions called) that the storage
    # (incl. removeedges) was send to the other processes (via edges_alltoall!)
    last_transmit::Int64
    # the last time that the edgetype was writable
    last_change::Int64
    # is set to true, when this field is in the read vector of a
    # transition function
    readable::Bool
    # here we track the agent on the target side of the edges, so
    # that in a case that an agent died, we can call
    # remove_edges! for the edges with the died agent at the source
    # position. We create a dict entry only for agents that are mortal.
    agentsontarget::Dict{AgentID, Vector{AgentID}}
end

"""
    create_model(types::ModelTypes, name::String)

Creates a struct that is a subtype of the `Simulation` type and methods
corresponding to the type information of `types` to the Julia
session. The new structure is named `name`, and all methods are
specific to this structure using Julia's multiple dispatch concept, so
it is possible to have different models in the same Julia session (as
long as `name` is different).

Returns a [`Model`](@ref) that can be used in
[`create_simulation`](@ref) to create a concrete simulation.  
"""
function create_model(typeinfos::ModelTypes, name::String)
    simsymbol = Symbol(name)

    edgefields = [
        Expr(Symbol("="),
             :($(Symbol(T))::EdgeFields{$(edgefield_type(T, typeinfos.edges_attr[T])),
                                        $(edgestorage_type(T, typeinfos.edges_attr[T]))}),
             :(EdgeFields($(edgefield_constructor(T, typeinfos.edges_attr[T])),
                          $(edgefield_constructor(T, typeinfos.edges_attr[T])),
                          $(edgestorage_type(T, typeinfos.edges_attr[T])()),
                          Vector{Tuple{AgentID, AgentID}}[],
                          [ [ Set{AgentID}() for _ in 1:mpi.size ] for _ in 1:MAX_TYPES ],
                          0,0,false,
                          Dict{AgentID, Vector{AgentID}}())))
        for T in typeinfos.edges_types ] 

    nodefields = [
        Expr(Symbol("="),
             :($(Symbol(T))::AgentFields{$T}),
             :(AgentFields($T)))
        for T in typeinfos.nodes_types ]

    fields = Expr(:block,
                  :(model::Model),
                  :(name::String),
                  :(filename::String),
                  :(overwrite_file::Bool),
                  :(params::P),
                  :(globals::G),
                  :(globals_last_change::Int64),
                  :(typeinfos::ModelTypes),
                  :(rasters::Dict{Symbol, Array}),
                  :(initialized::Bool),
                  :(intransition::Bool),
                  # after a transition this is the next transition nr
                  # in a transtion this is the current transtion nr
                  :(num_transitions::Int64),
                  :(logger::Log),
                  :(h5file::Union{HDF5.File, Nothing}),
                  :(external::Dict{Any, Any}),
                  edgefields...,
                  nodefields...)
    
    # the true in the second arg makes the struct mutable
    strukt = Expr(:struct, true, :($simsymbol{P, G} <: Simulation), fields)

    kwdefqn = QuoteNode(Symbol("@kwdef"))
    # nothing in third argument is for the expected LineNumberNode
    # see also https://github.com/JuliaLang/julia/issues/43976
    Expr(:macrocall, Expr(Symbol("."), :Base, kwdefqn), nothing, strukt) |> eval
    immortal = [ :Immortal in typeinfos.nodes_attr[T][:hints]
                 for T in typeinfos.nodes_types ]

    # Construct all type specific functions for the edge typeinfos
    for T in typeinfos.edges_types
        construct_edge_methods(T, typeinfos, simsymbol, immortal)
    end

    # Construct all type specific functions for the agent typeinfos
    for T in typeinfos.nodes_types
        construct_agent_methods(T, typeinfos, simsymbol)
    end

    construct_prettyprinting_methods(simsymbol)

    ### Parameters
    paramfields = [
        Expr(Symbol("="),
             :($(param.name)::$(typeof(param.default_value))),
             :($(param.default_value)))
        for param in typeinfos.params ]

    # the true in the second arg makes the struct mutable
    paramstrukt = Expr(:struct, true, :($(Symbol("Params_" * String(simsymbol)))),
                       Expr(:block, paramfields...))

    Expr(:macrocall, Expr(Symbol("."), :Base, kwdefqn), nothing, paramstrukt) |> eval    

    ### Globals
    globalfields = [
        Expr(Symbol("="),
             :($(g.name)::$(typeof(g.init_value))),
             :($(g.init_value)))
        for g in typeinfos.globals ]

    # the true in the second arg makes the struct mutable
    globalstrukt = Expr(:struct, true, :($(Symbol("Globals_" * String(simsymbol)))),
                        Expr(:block, globalfields...))

    Expr(:macrocall, Expr(Symbol("."), :Base, kwdefqn), nothing, globalstrukt) |> eval

    Model(typeinfos, name, immortal)
end

num_agenttypes(sim) = length(sim.typeinfos.nodes_types)

"""
    create_simulation(model::Model, [params = nothing, globals = nothing; name = model.name, filename = name, overwrite_file = true, logging = false, debug = false])

Creates and return a new simulation object, which stores the complete state 
of a simulation. 

`model` is an `Model` instance created by [`create_model`](@ref).

`params` must be a struct (or `nothing`) that contains all parameters of a
simulation. Parameter values are constant in a simulation run and can be
retrieved via the [`param`](@ref) function.

`globals` must be a mutable struct (or `nothing`). The values of these fields are
accessible for all agents via the [`get_global`](@ref) function. The values can
be changed by calling [`set_global!`](@ref) or [`push_global!`](@ref). 

The optional keyword argument `name` is used as meta-information about
the simulation and has no effect on the dynamics, since `name` is not
accessible in the transition functions. If `name` is not given, the
name of the model is used instead.

The optional `filename` keyword is a string that will be used as the
name of the hdf5 file when a simulation or a part of it is written via
e.g. [`write_snapshot`](@ref). 

The file is created when a `write...` function is called for the first time,
and it is created in an `h5` subfolder. By default an existing file
with the same `filename` will be overwritten, but this behavior can be
disabled with the `overwrite_file` keyword, in which case the files
will be numbered. If `filename` is not provided, the `name` argument
is used instead.

The keywords `logging` is a boolean flag that enables an automatical
log file.  The log file contains information such as the time spent in
different functions. When also `debug` is set to true, the log file
contains more details and the stream will be flushed after each write.

As with the hdf5 files, overwrite_file `the` keyword determines
whether the log files are overwritten or numbered. The numbering is
set independently from the numbering of the hdf5 files.

The simulation starts in an uninitialized state. After adding the
agents and edges for the initial state, it is necessary to call
[`finish_init!`](@ref) before applying a transition function for the first
time.

See also [`create_model`](@ref), [`param`](@ref),
[`get_global`](@ref), [`set_global!`](@ref), [`push_global!`](@ref)
and [`finish_init!`](@ref)
"""
function create_simulation(model::Model,
                    params::P = nothing,
                    globals::G = nothing;
                    name = model.name,
                    filename = name, overwrite_file = true,
                    logging = false, debug = false) where {P, G}

    simsymbol = Symbol(model.name)
    
    # Since v1.1 params and globals should be added to the model
    # via register_param! and register_global!
    # But to be backward compatible, it's still possible to
    # use own Param and Global Structs
    pstrukt = eval(Symbol("Params_" * model.name))
    @assert(params === nothing || fieldcount(pstrukt) == 0, """\n
    You can not use `register_param!` and the `param` argument of 
    `create_simulation` at the same time.
    """)

    params = if fieldcount(pstrukt) == 0
        params
    else
        deepcopy(pstrukt())
    end

    gstrukt = eval(Symbol("Globals_" * model.name))
    @assert(globals === nothing || fieldcount(gstrukt) == 0, """\n
    You can not use `register_globals!` and the `global` argument of 
    `create_simulation` at the same time.
    """)

    globals = if fieldcount(gstrukt) == 0
        globals
    else
        deepcopy(gstrukt())
    end

    sim::Simulation = @eval $(Symbol(model.name))(
        model = $model,
        name = $name,
        filename = $filename,
        overwrite_file = $overwrite_file,
        params = $params,
        globals = $globals,
        globals_last_change = 0,
        typeinfos = $(model.types),
        rasters = Dict{Symbol, Array}(),
        initialized = false,
        intransition = false,
        num_transitions = 0,
        logger = create_logger($name, $logging, $debug, $overwrite_file),
        h5file = nothing, # we need the sim instance to create the h5file
        # allows the client to attach arbitrary information, that will
        # be removed in finish_simulation.
        external = Dict{Any, Any}() 
    )

    for T in sim.typeinfos.edges_types
        init_field!(sim, T)
        init_storage!(sim, T)
    end

    for T in sim.typeinfos.nodes_types
        init_field!(sim, T)
    end

    global show_second_edge_warning = true

    with_logger(sim) do
        @info "New simulation created" name params date=Dates.now() starttime=time()
    end

    # using the finalizer in a parallel simulation can cause deadlocks
    if ! mpi.active
        finalizer(finish_simulation!, sim)
    end

    sim
end

# pipeable versions 
create_model(name::String) = types -> create_model(types, name)

create_simulation(params::P = nothing,
                  globals::G = nothing;
                  kwargs...) where {P, G} =
                      model -> create_simulation(model, params, globals; kwargs...)


function _create_equal_partition(part_dict, ids)
    # Partitions a sequence  into `n` nearly equally sized blocks.
    function partition(seq, n::Int)
        s, r = divrem(length(seq), n)
        
        [ ((i - 1) * s + 1 + min(i - 1, r)) : (i * s + min(i, r)) for i in 1:n ]
    end

    ps = partition(1:length(ids), mpi.size)
    for (i, range) in enumerate(ps)
        for idx in range
            part_dict[ids[idx]] = i
        end
    end
end

"""
    finish_init!(sim::Simulation; [distribute = true, 
                 partition::Dict{AgentID, ProcessID}, 
                 partition_algo = :Metis])

Finish the initialization phase of the simulation. 

`partition` is an option keyword and allows to specify an assignment
of the agents to the individual MPI ranks. The dictonary must contain
all agentids created on the rank as key, the corresponding value is
the rank on which the agent "lives" after `finish_init!`.

In the case that no `partition` is given and `distribute` is set to true,
the Graph will be partitioned with the given `partition_algo`. Currently
two algorithms are supported:
    - :Metis uses the Metis library for the graph partitioning. 
    - :EqualAgentNumbers just ensures that per agent type more or less
      the same number of agents are distributed to each process.

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
[`apply!`](@ref) and [`finish_simulation!`](@ref)
"""
function finish_init!(sim;
               partition = Dict{AgentID, ProcessID}(),
               return_idmapping = false, partition_algo = :Metis, distribute = true)
    @assert ! sim.initialized "You can not call finish_init! twice for the same simulation"

    _log_info(sim, "<Begin> finish_init!")

    sorted_pairs = sort(collect(sim.typeinfos.nodes_type2id), by = x -> x[2])
    foreach(finish_write!(sim), map(x -> x[1], sorted_pairs))
    foreach(finish_write!(sim), sim.typeinfos.edges_types)

    if distribute
        idmapping = if mpi.size > 1
            # we are creating an own partition only when no partition is given
            if length(partition) == 0 && mpi.isroot
                if partition_algo == :Metis
                    _log_info(sim, ">>> Partitioning the Simulation with Metis")
                    vsg = _log_time(sim, "create simple graph", true) do 
                        vahanasimplegraph(sim; show_ignorefrom_warning = false)
                    end
                    part = _log_time(sim, "call Metis.partition", true) do
                        # it's possible to load the graph after the initialization
                        # in this case we have nothing to partition here.
                        if Graphs.nv(vsg) > 0
                            Metis.partition(vsg, mpi.size; alg = :RECURSIVE)
                        end
                    end
                    _log_debug(sim, "<Begin> remap ids")
                    for (i, p) in enumerate(part)
                        partition[vsg.g2v[i]] = p
                    end
                    _log_debug(sim, "<End> remap ids")
                elseif partition_algo == :EqualAgentNumbers
                    _log_info(sim, ">>> Partitioning the Simulation / equal number of nodes per type")
                    _log_debug(sim, "<Begin> create equal partitions")
                    for T in sim.typeinfos.nodes_types
                        ids = map(i -> agent_id(sim, AgentNr(i), T),
                                  keys(readstate(sim, T)))
                        _create_equal_partition(partition, ids)
                    end
                    _log_debug(sim, "<End> create equal partitions")
                else
                    @error "the partition_algo given is unknown"
                end
            end
            if mpi.isroot
                _log_info(sim, ">>> Distributing the Simulation")
            end
            _log_time(sim, "distribute!") do
                distribute!(sim, partition)
            end
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
    end

    sim.initialized = true
    sim.num_transitions = 1
    _log_info(sim, "<End> finish_init!")
    distribute && return_idmapping ? idmapping : nothing
end 

# this function is not exported and should be only used for unit tests
function updateids(idmap, oldids)
    map(oldids) do id
        idmap[Vahana.remove_process(id)]
    end
end


"""
    copy_simulation(sim)

Create an independent copy of the simulation `sim`. Since part of the
memory is allocated via MPI, you should not use deepcopy. 

Also, the copy is detached from any output or logger file so that not
sim and copy tried to write to the same file(s). If these are needed,
you can create these files for the copy by calling
[`create_h5file!`](@ref) and/or [`create_logger!`](@ref) and assigning
the results to the h5file/logging field of the returned simulation.

Returns the copy of the simulation.
"""
function copy_simulation(sim)
    copy = deepcopy(sim)
    # we also copied the MPI windows, so the copy points to the same
    # memory as the original. So this needs to be fixed.
    foreach(T -> copy_shm!(copy, T), sim.typeinfos.nodes_types)
    copy.h5file = nothing
    # disable logging for the copy
    copy.logger = create_logger(sim.name, false, false, sim.overwrite_file)

    copy
end


function _free_memory!(sim)
    for T in sim.typeinfos.nodes_types
        shmstatewin = windows(sim, T).shmstate
        if ! isnothing(shmstatewin)
            MPI.free(shmstatewin)
            windows(sim, T).shmstate = nothing
        end
        shmdiedwin = windows(sim, T).shmdied
        if ! isnothing(shmdiedwin)
            MPI.free(shmdiedwin)
            windows(sim, T).shmdied = nothing
        end
        init_field!(sim, T)
    end       
    for T in sim.typeinfos.edges_types
        empty!(edgeread(sim, T))
        empty!(edgewrite(sim, T))
    end
    empty!(sim.rasters)
end

"""
    finish_simulation!(sim)

This function removes all agents/edges and rasters from the simulation to minimize
the memory footprint, closes all open files and finalizes MPI. The
`parameters` and `globals` of the simulation are still available, the
remaining state of the simulation is undefined.


!!! warning

    In a REPL session the function serves as a finalizer for the simulation object
    ('sim'). But for parallel simulation it is essential that this function is
    called manually before the termination of the simulation.

Returns the `globals` of the simulation.
"""
function finish_simulation!(sim)
    _log_info(sim, "<Begin> finish_simulation!")

    if ! MPI.Finalized()
        _free_memory!(sim)
    end
    
    close_h5file!(sim)

    empty!(sim.external)

    _log_info(sim, "<End> finish_simulation!")
    
    if sim.logger.file !== nothing
        close(sim.logger.file)
    end

    if ! MPI.Finalized()
        MPI.Barrier(MPI.COMM_WORLD)
    end

    sim.globals
end


######################################## Transition


"""
    param(sim::Simulation, name)

Returns the value of the field `name` of the `params` struct from the
Simulation constructor.

See also [`create_model`](@ref)
"""
param(sim, name) = getfield(sim.params, name)

"""
    set_param!(sim::Simulation, param::Symbol, value)

Assign the specified `value` to the `param` parameter. This operation
is only allowed prior to calling the `finish_init!` method.

A pipeable version of `set_param!` is also available.

"""
function set_param!(sim::Simulation, param::Symbol, value)
    @assert !sim.initialized """\n
         You can call `set_param!` only before `finish_init!`. For values that 
         change while the simulation is running, register them as globals and use 
         `set_global!` instead.
             """
    if ! hasproperty(sim.params, param)
        @warn "Parameter $(param) is unknown" _file=nothing _line=nothing _module=nothing
    else
        setfield!(sim.params, param, value)
    end
end

function set_param!(param::Symbol, value)
    sim -> begin
        set_param!(sim, param, value)
        sim
    end
end

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

prepare_write!(sim, read, add_existing) =
    t -> prepare_write!(sim, read, t in add_existing, t)

finish_write!(sim) = t -> finish_write!(sim, t)

isiterable(v) = applicable(iterate, v)


"""
    apply!(sim, func, call, read, write; [add_existing, with_edge])

Apply the transition function `func` to the simulation state. 

`call` must be a single agent type or a collection of agent types,
likewise `read` and `write` must be a single agent/edge type or a
collection of agent/edge types.

`call` determines for which agent types the transition function `func`
is called. Within the transition function, an agent has access to the
state of agents (including its own state) and to edges only if
their types are in the `read` collection. Accordingly, the agent
can change its own state and/or create new agents or edges only if
their types are in the `write` collection.

Assume that T is an agent type that is in `call`. In case T is also in
`read`, the transition function must have the following signature:
`transition_function(agent::T, id, sim)`, where the type declaration
of agent is optional if `call` contains only a single type. If T is
not in `read`, it must have the signature `transition_function(::Val{T},
id::AgentID, sim::Simulation)`.

If T is in `write`, the transition function must return either an agent of type T
or `nothing`. If `nothing` is returned, the agent will be removed from
the simulation, otherwise the agent with id `id` will get the returned
state after the transition function was called for all agents.

When an edge state type is in `write`, the current edges of that type
are removed from the simulation. If you want to keep the existing
edges for a specific type, you can add this type to the optional
`add_existing` collection.

Similarly, agent types that are in the `write` but not in the `call`
argument can be part of the `add_existing` collection, so that the
existing agents of this type are retained and can only be added. For
agent types in `call`, however, this is achieved by returning their
state in the transition function.

With the keyword `with_edge` it is possible to restrict the set of
agents for which the transition function is called to those agents who are on
the target side of edges of the type `with_edge`. So  
```
apply!(sim, AT, ET, []) do _, id, sim 
    if has_edge(sim, id, ET)
       do_something
    end
end
```
is equivalent to 
```
apply!(sim, AT, ET, []; with_edge = ET) do _, id, sim 
    do_something
end
```
but saves all the `has_edge` checks.

This `with_edge` keyword should only be set when a small subset of
agents possess edges of this particular type, as performance will be
adversely impacted in other scenarios. Also the keyword can only be
used for edgetypes without the :SingleType hint.

See also [`apply`](@ref) and the [Applying Transition Function section
in the tutorial](tutorial1.md#Applying-Transition-Functions)
"""
function apply!(sim::Simulation,
         func::Function,
         call,
         read,
         write;
         add_existing = [],
         with_edge = nothing)
    @assert sim.initialized "You must call finish_init! before apply!"

    @assert with_edge === nothing || !has_hint(sim, with_edge, :SingleType) """
    The `with_edge` keyword can only be used for edgetypes without the
    :SingleType hint.
    """

    with_logger(sim) do
        @info "<Begin> apply!" func transition=sim.num_transitions+1
    end
    
    # must be set to true before prepare_read! (as this calls add_edge!)
    sim.intransition = true

    # Do allow also just use a single type as argument, we check this
    # here and convert the single type to a vector if necessary
    call = applicable(iterate, call) ? call : [ call ]
    read = applicable(iterate, read) ? read : [ read ]
    write = applicable(iterate, write) ? write : [ write ]
    add_existing = applicable(iterate, add_existing) ?
        add_existing : [ add_existing ]

    for c in call
        @assert ! (c in add_existing) "$c can not be element of `add_existing`"
    end
    for ae in add_existing
        @assert ae in write "$ae is in `add_existing` but not in `write`"
    end

    readableET = filter(w -> w in sim.typeinfos.edges_types, read)
    foreach(T -> prepare_read!(sim, read, T), readableET)
    readableAT = filter(w -> w in sim.typeinfos.nodes_types, read)
    foreach(T -> prepare_read!(sim, read, T), readableAT)

    writeableAT = filter(w -> w in sim.typeinfos.nodes_types, write)
    writeableET = filter(w -> w in sim.typeinfos.edges_types, write)
    
    for T in writeableET
        if ! (T in add_existing)
            empty!(agentsontarget(sim, T))
        end
    end
    
    foreach(prepare_write!(sim, read, [call; add_existing]), write)

    MPI.Barrier(MPI.COMM_WORLD)

    for C in call
        with_logger(sim) do
            @debug "<Begin> tf_call!" agenttype=C transition=sim.num_transitions+1
        end
        wfunc = C in write ? transition_with_write! : transition_without_write!
        if with_edge === nothing
            rfunc = C in read ? transition_with_read! : transition_without_read!
            rfunc(wfunc, sim, func, C)
        else
            rfunc = C in read ? transition_with_read_with_edge! :
                transition_without_read_with_edge!
                rfunc(wfunc, sim, func, C, with_edge)
        end    
        _log_debug(sim, "<End> tf_call!")
    end
    
    MPI.Barrier(MPI.COMM_WORLD)

    # we first remove all deleted edges, so that we can readd
    # new edges to an agent in the SingleEdge case.
    # This include the edges of died agents.
    foreach(ET -> transmit_remove_edges!(sim, ET), writeableET)
    
    # we must first call transmit_edges, so that they are exists
    # on the correct rank instead in @storage, where they are
    # not deleted in the case that an agent died
    foreach(ET -> transmit_edges!(sim, ET), writeableET)
    
    foreach(T -> finish_read!(sim, T), read)

    # then we can call finish_write! for the agents, as this will
    # remove the edges where are died agents are involved (and modifies
    # EdgeType_write).
    foreach(finish_write!(sim), writeableAT)

    foreach(finish_write!(sim), writeableET)

    sim.intransition = false
    
    # must be incremented after the transition, so that read only
    # functions like mapreduce tries to transfer the necessary states
    # only once 
    sim.num_transitions = sim.num_transitions + 1

    _log_info(sim, "<End> apply!")
    
    sim
end

function apply!(func::Function,
         sim::Simulation,
         call,
         read,
         write;
         kwargs ...)
    apply!(sim, func, call, read, write; kwargs ...)
end


"""
    apply!(sim, func, call, read, write; add_existing)

Call [`apply!`](@ref) with a copy of the simulation so that the
state of `sim` itself is not changed.

Can be very useful during development, especially if Vahana is used in
the REPL. However, for performance reasons, this function should not
be used in the final code.

Returns the copy of the simulation.

See also [`apply!`](@ref)
"""
function apply(sim::Simulation,
        func::Function,
        call,
        read,
        write;
        kwargs ...) 
    newsim = copy_simulation(sim)
    apply!(newsim, func, call, read, write; kwargs ...)
    newsim
end

function apply(func::Function,
        sim::Simulation,
        call,
        read,
        write;
        kwargs ...)
    apply(sim, func, call, read, write; kwargs ...)
end

######################################## mapreduce


import Base.mapreduce
"""
    mapreduce(sim, f, op, ::Type{T}; [kwargs ...])

Calculate an aggregated value, based on the state of all agents or
edges of type T.

`f` is applied to all of these agents or edges and then `op` is used to
reduce (aggregate) the values returned by `f`.

mapreduce is calling Base.mapreduce, `f`, `op` and `kwargs` are
passed directly to mapreduce, while `sim` and `T` are used to determine the
iterator.
"""
function mapreduce(::Simulation, f, op, ::Type; kwargs...) end
