Base.@kwdef struct EdgeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    init
    add_edge
    edges_to
    prepare_write
    finish_write
    aggregate
end

#################### Edge Dict

eff_dict = EdgeFieldFactory(
    type = (T, _) -> :(Dict{AgentID, Vector{Edge{Main.$T}}}),
    constructor = (T, _) -> :(Dict{AgentID, Vector{Edge{Main.$T}}}()),
    init = (_, _) -> :(),

    add_edge = (T, _) ->
        :(function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
              push!(get!(Vector{Main.$T}, sim.$(writefield(T)), to), edge)
          end),

    edges_to = (T, _) ->
        :(function edges_to(sim, to::AgentID, ::Val{Main.$T}) 
              get(Vector{Main.$T}, sim.$(readfield(T)), to)
          end),

    prepare_write = (T, _) ->
        :(function prepare_write!(sim, ::Val{Main.$T})
              sim.$(writefield(T)) = Dict{AgentNr, Vector{Edge{Main.$T}}}()
          end),

    finish_write = (T, _) ->
        :(function finish_write!(sim, ::Val{Main.$T})
              sim.$(readfield(T)) = sim.$(writefield(T))
          end),

    aggregate = (T, _) -> begin
        :(function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
              estates = sim.$(readfield(T)) |>
                  values |>
                  Iterators.flatten |>
                  collect |>
                  edgestates
              mapreduce(f, op, estates; kwargs...)
          end)
    end, 
)

#################### EdgeFieldFactory Dict

effs = Dict(:Dict => eff_dict)

