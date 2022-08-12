export construct_model
export new_simulation
export finish_init!
export apply_transition, apply_transition!
export param
export aggregate

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
function construct_model(types::ModelTypes, name::String) 
    simsymbol = Symbol(name)
    
    edgefields = [
        map(["_read", "_write"]) do RW
            Expr(Symbol("="),
                 :($(Symbol(T, RW))::$(edgefield_type(T, types.edges_attr[T]))),
                 :($(edgefield_constructor(T, types.edges_attr[T]))))
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
                  :(modelname::String),
                  :(name::String),
                  :(params::P),
                  :(globals::G),
                  :(typeinfos::ModelTypes),
                  :(rasters::Dict{Symbol, Array}),
                  :(nodes_id2read::Vector{Function}),
                  :(nodes_id2write::Vector{Function}),
                  :(initialized::Bool),
                  edgefields...,
                  nodefields...,
                  nodeids...)
    
    # the true in the second arg makes the struct mutable
    strukt = Expr(:struct, true, :($simsymbol{P, G}), fields)

    
    kwdefqn = QuoteNode(Symbol("@kwdef"))
    # nothing in third argument is for the expected LineNumberNode
    # see also https://github.com/JuliaLang/julia/issues/43976
    Expr(:macrocall, Expr(Symbol("."), :Base, kwdefqn), nothing, strukt) |> eval

    # Construct all type specific functions for the edge types
    for T in types.edges_types
        construct_edge_functions(T, types.edges_attr[T], simsymbol)
    end

    # Construct all type specific functions for the agent types
    for (T, C) in types.nodes
        nffs[C].init_field(T, types, simsymbol) 
        nffs[C].add_agent(T, types, simsymbol)  
        nffs[C].agentstate(T, types, simsymbol) 
        nffs[C].prepare_write(T, types, simsymbol) 
        nffs[C].transition(T, types, simsymbol) 
        nffs[C].finish_write(T, types, simsymbol) 
        nffs[C].aggregate(T, types, simsymbol) 
    end

    for T in types.nodes_types
        types.nodes_id2read[types.nodes_type2id[T]] =
            @eval sim -> sim.$(readfield(Symbol(T)))
        types.nodes_id2write[types.nodes_type2id[T]] =
            @eval sim -> sim.$(writefield(Symbol(T)))
    end

    
    construct_prettyprinting_functions(simsymbol)

    Model(types, name)
end


"""
    new_simulation(model::Model, params, globals; name)

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
function new_simulation(model::Model, params::P, globals::G; name = model.name) where {P, G}
    sim = @eval $(Symbol(model.name))(
        modelname = $(model.name),
        name = $name,
        params = $params,
        globals = $globals,
        typeinfos = $(model.types),
        rasters = Dict{Symbol, Array}(),
        initialized = false,
        nodes_id2read = $(model.types.nodes_id2read),
        nodes_id2write = $(model.types.nodes_id2write)
    )

    for T in sim.typeinfos.edges_types
        init_field!(sim, T)
    end

    for T in sim.typeinfos.nodes_types
        init_field!(sim, T)
    end

    sim
end

# pipeable versions 
construct_model(name::String) = types -> construct_model(types, name)

new_simulation(params::P, globals::G; kwargs...) where {P, G} =
    model -> new_simulation(model, params, globals; kwargs...)

# construct(name::String, params::P, globals::G) where {P, G} =
#     types -> construct(types, name, params, globals)


"""
    finish_init!(sim::Simulation)

Finish the initialization phase of the simulation. 

Must be called before applying a transition function. 

See also [`register_agenttype!`](@ref), [`register_edgetype!`](@ref) and
[`apply_transition!`](@ref)
"""
function finish_init!(sim;
               partition::Dict{AgentID, ProcessID} =
                   Dict{AgentID, ProcessID}())
    foreach(finish_write!(sim), keys(sim.typeinfos.nodes_type2id))
    foreach(finish_write!(sim), sim.typeinfos.edges_types)
    sim.initialized = true
    sim
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
    apply_transition!(sim, func, compute, networks, rebuild; invariant_compute = false, add_existing = Vector{DataType}())

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

`network` is a vector of Edge types. Inside of `func` all the
functions like [`edges_to`](@ref) that access information about edges
can be only called when the edge type is in the `network` vector.

`rebuild` is a vector of Agent and/or Edge types. All the instances of
agents or edges with a type in (`rebuild` - `add_existing`) will be
removed and must be added again inside of `func`.
[`add_agent!`](@ref), [`add_agents!`](@ref), [`add_edge!`](@ref) and
[`add_edges!`](@ref) can be only called for types in `rebuild` or
`compute`. In the case, that a transition function should only add additional
edges for some types, these types must be listed in the `add_existing`
vector.

See also [`apply_transition`](@ref)
"""
function apply_transition!(sim,
                    func::Function,
                    compute::Vector,
                    networks::Vector,
                    rebuild::Vector;
                    invariant_compute = false,
                    add_existing = Vector{DataType}())
    writeable = invariant_compute ? rebuild : [ compute; rebuild ]

    foreach(prepare_write!(sim, add_existing), writeable)

    if invariant_compute
        for C in compute
            transition_edges_only!(sim, func, C)
        end
    else
        for C in compute
            transition!(sim, func, C)
        end
    end

    foreach(finish_write!(sim), writeable)

    sim
end

function apply_transition!(func::Function,
                    sim,
                    compute::Vector,
                    networks::Vector,
                    rebuild::Vector;
                    kwargs ...)
    apply_transition!(sim, func, compute, networks, rebuild; kwargs ...)
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
"""
function aggregate(::__MODEL__, f, op, ::Type; kwargs...) end
