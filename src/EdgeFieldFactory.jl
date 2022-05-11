Base.@kwdef struct EdgeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    # Functions:
    init_field = (T, _) -> @eval init_field!(_, ::Val{Main.$T}) = nothing
    add_edge
    edges_to
    prepare_write = (T, _) -> @eval prepare_write!(_, ::Val{Main.$T}) = nothing
    finish_write = (T, _) -> begin 
        @eval function finish_write!(sim, ::Val{Main.$T})
            sim.$(readfield(T)) = sim.$(writefield(T))
        end
    end 
    aggregate
    num_neighbors = (T, _) -> begin
        @eval function num_neighbors(sim, to::AgentID, t::Val{Main.$T})
            if haskey(sim.$(readfield(T)), to)
                length(sim.$(readfield(T))[to])
            else
                0
            end
        end
    end
end

#################### Edge Dict

eff_dict = EdgeFieldFactory(
    type = (T, _) -> :(Dict{AgentID, Vector{Edge{Main.$T}}}),
    constructor = (T, _) -> :(Dict{AgentID, Vector{Edge{Main.$T}}}()),

    add_edge = (T, _) -> begin
        @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
            push!(get!(Vector{Edge{Main.$T}}, sim.$(writefield(T)), to), edge)
        end
    end,

    edges_to = (T, _) -> begin
        @eval function edges_to(sim, to::AgentID, ::Val{Main.$T}) 
            get(Vector{Edge{Main.$T}}, sim.$(readfield(T)), to)
        end
    end,



    prepare_write = (T, _) -> begin
        @eval function prepare_write!(sim, ::Val{Main.$T})
            sim.$(writefield(T)) = Dict{AgentNr, Vector{Edge{Main.$T}}}()
        end
    end,

    aggregate = (T, _) -> begin
        @eval function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
            estates = sim.$(readfield(T)) |>
                values |>
                Iterators.flatten |>
                collect |>
                edgestates
            mapreduce(f, op, estates; kwargs...)
        end
    end 
)


#################### Edge Dict

eff_stateless = EdgeFieldFactory(
    type = (_, _) -> :(Dict{AgentID, Vector{AgentID}}),
    constructor = (_, _) -> :(Dict{AgentID, Vector{AgentID}}()),

    add_edge = (T, _) -> begin
        @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
            push!(get!(Vector{AgentID}, sim.$(writefield(T)), to), edge.from)
        end
        @eval function add_edge!(sim, from::AgentID, to::AgentID, ::Type{Main.$T})
            push!(get!(Vector{AgentID}, sim.$(writefield(T)), to), from)
        end
        @eval function add_edge!(sim, from::AgentID, to::AgentID, state::Main.$T)
            push!(get!(Vector{AgentID}, sim.$(writefield(T)), to), from)
        end
    end,

    edges_to = (T, _) -> begin
        @eval function edges_to(sim, to::AgentID, ::Val{Main.$T}) 
            get(Vector{AgentID}, sim.$(readfield(T)), to)
        end
        @eval neighborstates(sim, to::AgentID, edgetype::Val{Main.$T}, agenttype::Val) = 
            map(e -> agentstate(sim, e, agenttype), edges_to(sim, id, edgetype))  
        @eval neighborstates_flexible(sim, id::AgentID, edgetype::Val{Main.$T}) =
            map(e -> agentstate_flexible(sim, e), edges_to(sim, id, edgetype))  
    end,

    prepare_write = (T, _) -> begin
        @eval function prepare_write!(sim, ::Val{Main.$T})
            sim.$(writefield(T)) = Dict{AgentNr, Vector{AgentID}}()
        end
    end,

    aggregate = (T, _) -> begin
        @eval function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
            @assert false "aggregate can not be called for Stateless edges"
        end
    end 

)

#################### EdgeFieldFactory Dict

effs = Dict(:Dict => eff_dict, :Stateless => eff_stateless)

