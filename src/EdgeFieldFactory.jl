Base.@kwdef struct EdgeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    init
    add_edge
    edges_to
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
          end) 
)

#################### Edge Vec

# function _add_edge_vec!(coll, to, edge::T) where T
#     s = size(coll, 1)
#     if s < to
#         resize!(coll, to)
#         for i in (s+1):to
#             coll[i] = Vector{T}()
#         end
#     end
#     push!(coll[to], edge)
# end

# function _edges_to_vec!(coll::Vector{T}, to) where T
#     s = size(coll, 1)
#     if s < to
#         return Vector{T}()
#     else
#         coll[to]
#     end
# end

# eff_vector = EdgeFieldFactory(
#     type = (T, info) -> :(Vector{Vector{Edge{Main.$T}}}),
#     constructor = (T, info) -> :(Vector{Vector{Edge{Main.$T}}}()),
#     init = (T, info) -> 
#         if haskey(info.edges_attr[T], :size)
#             s = info.edges_attr[T][:size]
#             :(function init_type!(sim, ::Val{Symbol(Main.$T)})
#                   resize!(sim.$(writefield(T)), $s)
#                   for i in 1:$s
#                       sim.$(writefield(T))[i] = Vector{Main.$T}()
#                   end
#               end)
#         else
#             :()
#         end,

#     # we use in the following agent_nr instead of id. We have already
#     # different fields for the differnt types, and also different vector
#     # on the differnt PEs, so there will be no conflict 
#     add_edge = (T, info) ->
#         if haskey(info.edges_attr[T], :size)
#             :(function add_edge!(sim, to, edge::Edge{Main.$T})
#                   push!(sim.$(writefield(T))[agent_nr(to)], edge)
#               end)
#         else
#             :(function add_edge!(sim, to, edge::Edge{Main.$T})
#                   _add_edge_vec!(sim.$(writefield(T)), agent_nr(to), edge)
#               end)
#         end,

#     edges_to = (T, info) ->
#         if haskey(info.edges_attr[T], :size)
#             :(function edges_to(sim, to::Int64, ::Val{Main.$T})
#                   sim.$(readfield(T))[agent_nr(to)]
#               end)
#         else
#             :(function edges_to(sim, to::Int64, ::Val{Main.$T})
#                   _edges_to_vec!(sim.$(readfield(T)), agent_nr(to))
#               end)
#         end    
# )

#################### EdgeFieldFactory Dict

effs = Dict(:Dict => eff_dict)
#            :Vector => eff_vector)

