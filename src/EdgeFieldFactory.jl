Base.@kwdef struct EdgeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    init
    add_edge
    edges_to
end

#################### Edge Dict

eff_dict = EdgeFieldFactory(
    type = (T, _) -> :(Dict{Int64, Vector{Edge{Main.$T}}}),
    constructor = (T, _) -> :(Dict{Int64, Vector{Edge{Main.$T}}}()),
    init = (_, _) -> :(),

    add_edge = (T, _) ->
        :(function add_edge!(sim::Simulation, to::AgentID, edge::Edge{Main.$T})
              push!(get!(Vector{Main.$T}, sim.$(writefield(T)), to), edge)
          end),

    edges_to = (T, _) ->
        :(function edges_to(sim::Simulation, to::AgentID, ::Val{Main.$T}) 
              get(Vector{Main.$T}, sim.$(readfield(T)), to)
          end) 
)

#################### Edge Vec

function _add_edge_vec!(coll, to, edge::T) where T
    s = size(coll, 1)
    if s < to
        resize!(coll, to)
        for i in (s+1):to
            coll[i] = Vector{T}()
        end
    end
    push!(coll[to], edge)
end

eff_vector = EdgeFieldFactory(
    type = (T, info) -> :(Vector{Vector{Edge{$T}}}),
    constructor = (T, info) -> :(Vector{Vector{Edge{$T}}}()),
    init = (T, info) -> 
        if haskey(info.edges_attr[T], :size)
            s = info.edges_attr[T][:size]
            :(function init_type!(sim::Simulation, ::Val{Symbol($T)})
                  resize!(sim.$(writefield(T)), $s)
                  for i in 1:$s
                      sim.$(writefield(T))[i] = Vector{$T}()
                  end
              end)
        else
            :()
        end,

    add_edge = (T, info) ->
        if haskey(info.edges_attr[T], :size)
            :(function add_edge!(sim::Simulation, to, edge::Edge{$T})
                  push!(sim.$(writefield(T))[to], edge)
              end)
        else
            :(function add_edge!(sim::Simulation, to, edge::Edge{$T})
                  _add_edge_vec!(sim.$(writefield(T)), to, edge)
              end)
        end,

    edges_to = (T, info) ->
        :(function edges_to(sim::Simulation, to::Int64, ::Val{$T})
              sim.$(readfield(T))[to]
          end)
    
)

#################### EdgeFieldFactory Dict

effs = Dict(:Dict => eff_dict,
            :Vector => eff_vector)

