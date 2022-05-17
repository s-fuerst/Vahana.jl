export construct
export finish_init!
#export typeid
export apply_transition, apply_transition!
export param
export aggregate

const MAX_TYPES = typemax(TypeID) 

"""
    construct(types::ModelTypes, name::String, params, globals)

Create a new simulation object, which stores the complete state of a simulation. 

`types` TODO DOC

`name` is used as meta-information about the simulation and has no
effect on the dynamics, since `name` is not accessible in the
transition functions. 

`params` must be a struct (or `nothing`) that contains all parameters of a
simulation. Parameter values are constant in a simulation run and can be
retrieved via the [`param`](@ref) function.

`globals` must be a mutable struct (or `nothing`). The values of these fields are
accessible for all agents via the [`getglobal`](@ref) function. The values can
be changed by calling [`setglobal!`](@ref) or [`pushglobal!`](@ref). 

The simulation starts in an uninitialized state. After adding the
agents and edges for the initial state, it is necessary to call
[`finish_init!`](@ref) before applying a transition function for the first
time.

See also [`ModelTypes!`](@ref), [`param`](@ref),
[`getglobal`](@ref), [`setglobal!`](@ref), [`pushglobal!`](@ref)
and [`finish_init!`](@ref)
"""
function construct(types::ModelTypes, name::String, params::P, globals::G) where {P, G}
    edgefields = [
        map(["_read", "_write"]) do RW
            Expr(Symbol("="),
                 :($(Symbol(T, RW))::$(edgefield_type(T, types.edges_attr[Symbol(T)]))),
                 :($(edgefield_constructor(T, types.edges_attr[Symbol(T)]))))
        end 
        for T in types.edges_types ] |> Iterators.flatten |> collect

    nodefields = [
        map(["_read", "_write"]) do RW
            Expr(Symbol("="),
                 :($(Symbol(T, RW))::$(nffs[C].type(T, types))),
                 :($(nffs[C].constructor(T, types))))
        end 
        for (T,C) in types.nodes ] |> Iterators.flatten |> collect

    nodeids = [
        Expr(Symbol("="),
             :($(Symbol(T, "_nextid"))::AgentNr),
             :(AgentNr(1)))
        for (T,_) in types.nodes ]

    fields = Expr(:block,
                  :(name::String),
                  :(params::P),
                  :(globals::G),
                  :(typeinfos::ModelTypes),
                  :(rasters::Dict{Symbol, Array{AgentID,2}}),
                  :(nodes_id2read::Vector{Function}),
                  :(nodes_id2write::Vector{Function}),
                  edgefields...,
                  nodefields...,
                  nodeids...)
    
    # the true in the second arg makes the struct mutable
    strukt = Expr(:struct, true, :(Simulation{P, G}), fields)

    kwdefqn = QuoteNode(Symbol("@kwdef"))
    # nothing in third argument is for the expected LineNumberNode
    # see also https://github.com/JuliaLang/julia/issues/43976
    Expr(:macrocall, Expr(Symbol("."), :Base, kwdefqn), nothing, strukt) |> eval

    sim = @eval Simulation(name = $name,
                           params = $params,
                           globals = $globals,
                           typeinfos = $types,
                           rasters = Dict{Symbol, Array{AgentID,2}}(),
                           nodes_id2read = Vector{Function}(undef, MAX_TYPES),
                           nodes_id2write = Vector{Function}(undef, MAX_TYPES)
                           )

    # Construct all type specific functions for the edge types
    for T in sim.typeinfos.edges_types
        construct_edge_functions(Symbol(T), types.edges_attr[Symbol(T)])
    end

    # Construct all type specific functions for the agent types
    for (T, C) in sim.typeinfos.nodes
        nffs[C].init_field(T, sim.typeinfos) 
        nffs[C].add_agent(T, sim.typeinfos)  
        nffs[C].agentstate(T, sim.typeinfos) 
        nffs[C].prepare_write(T, sim.typeinfos) 
        nffs[C].transition(T, sim.typeinfos) 
        nffs[C].finish_write(T, sim.typeinfos) 
        nffs[C].aggregate(T, sim.typeinfos) 
    end

    @eval _init_all_types($sim)
end

function _init_all_types(sim)
    for T in sim.typeinfos.edges_types
        init_field!(sim, Val(T))
    end

    for T in sim.typeinfos.nodes_types
        init_field!(sim, Val(T))
        sim.nodes_id2read[sim.typeinfos.nodes_type2id[T]] =
            @eval sim -> sim.$(readfield(Symbol(T)))
        sim.nodes_id2write[sim.typeinfos.nodes_type2id[T]] =
            @eval sim -> sim.$(writefield(Symbol(T)))
    end

    sim
end

construct(name::String, params::P, globals::G) where {P, G} =
    types -> construct(types, name, params, globals)
    

"""
    finish_init!(sim::Simulation)

Finish the initialization phase of the simulation. 

Must be called before applying a transition function. All types of agents and
edges must be registered before `finish_init!` is called.

See also [`add_agenttype!`](@ref), [`add_edgetype!`](@ref) and
[`apply_transition!`](@ref)
"""
function finish_init!(sim)
    foreach(finish_write!(sim), keys(sim.typeinfos.nodes_type2id))
    foreach(finish_write!(sim), sim.typeinfos.edges_types)
    sim
end 


######################################## Transition


"""
    param(sim::Simulation, name)

Returns the value of the field `name` of the `params` struct from the
Simulation constructor.

See also [`construct`](@ref)
"""
param(sim, name) = getfield(sim.params, name)

function maybeadd(coll,
           id::AgentNr,
           agent) 
    # the coll[id] writes into another container then
    # we use for the iteration
    coll[id] = agent
    nothing
end

function maybeadd(_,
           ::AgentNr,
           ::Nothing)
    nothing
end

prepare_write!(sim) = t -> prepare_write!(sim, Val(t))

finish_write!(sim) = t -> finish_write!(sim, Val(t))


"""
    apply_transition!(sim, func, compute, networks, rebuild)

Apply the transition function `func` to the simulation state. 

A transition function must have the following signature: `function(agent::T,
id::AgentID, sim::Simulation)` and must return either an agent of type
T or `nothing`. If `nothing` is returned, the agent will be removed
from the simulation, otherwise the agent with id `id` will have
(starting with the next apply_transition! call) the returned state.

`compute` is a vector of Agent types. `func` is called for every agent
with a type that is listed in `compute`, so a method for each of this types
must be implemented.

`network` is a vector of Edge types. Inside of `func`
[`edges_to`](@ref) can be only called for types in `network`.

`rebuild` is a vector of Agent and/or Edge types. All the instances of
agents or edges with a type in `rebuild` will be removed and must be
added again inside of `func`. Also [`add_agent!`](@ref),
[`add_agents!`](@ref), [`add_edge!`](@ref) and [`add_edges!`](@ref)
can be only called for types in `network` or `compute`.

See also [`apply_transition`](@ref)
"""
function apply_transition!(sim,
                    func,
                    compute::Vector,
                    networks::Vector,
                    rebuild::Vector)
    writeable = [ compute; rebuild ]
    # writeable_nodes = intersect(writeable, keys(sim.typeinfos.nodes_type2id))
    # writeable_edges = intersect(writeable, sim.typeinfos.edges_types)

    foreach(prepare_write!(sim), writeable)

    for c in compute
        transition!(sim, func, Val(c))
    end

    foreach(finish_write!(sim), writeable)
#    foreach(finish_write_edge!(sim), writeable_edges)
    sim
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
                   rebuild::Vector) 
    newsim = deepcopy(sim)
    apply_transition!(newsim, func, compute, networks, rebuild)
    newsim
end

######################################## aggregate

"""
    aggregate(sim, ::Type{T}, f, op; kwargs ...)

Calculate an aggregated value, based on the state of all agents or
edges of type T.

`f` is applied to all of these agents or edges and then `op` is used to
reduce (aggregate) the values returned by `f`.

aggregate is based on [`Base.mapreduce`](@ref), `f`, `op` and `kwargs` are
passed directly to mapreduce, while `sim` and `T` are used to determine the
iterator.

See also [`Base.mapreduce`](@ref)
"""
function aggregate(sim, ::Val{T}, f, op; kwargs...) where T end


# function aggregate(sim, ::Val{T}, f, op;
#             kwargs...) where T
#     sim.nodes_id2read[sim.typeinfos.nodes_type2id[T]](sim) |>
    
#     agents = sim.agents[sim.agent_typeids[T]] |>
#         read_container |>
#         values
#     mapreduce(f, op, agents; kwargs...)
# end

# function aggregate(sim, ::Type{T}, f, op;
#             kwargs...) where T
#     edges = sim.edges[T] |>
#         read_container |>
#         values |>
#         Iterators.flatten |>
#         collect |>
#         edgestates
#     mapreduce(f, op, edges; kwargs...)
# end

