import Base.zero

# In the following comment:
# SType stands for the :SingleAgentType property
# SEdge stands for the :SingleEdge property
# and
# Ignore stands for the :IgnoreFrom property
#
# The overall form of the type for the edgefield is the string: A*C(B)}, where
#
# |                             | A             | B             | C(B)      |
# |-----------------------------+---------------+---------------+-----------|
# | Default                     | Dict{AgentID, | Edge{Main.$T} | Vector{B} |
# | SType                       | Vector{       |               | B         |
# | Stateless & !Ignore         |               | AgentID       |           |
# | Ignore & !Stateless         |               | Main.$T       |           |
# | Stateless & Ignore & !SEdge |               |               | Int       |
# | Stateless & Ignore & SEdge  |               |               | Bool      |
#
# The following table show all the 16 type variante for the container that can be
# constructed following the rules from the table above
#
# |                                      | SType | Stateless | SEdge | Ignore |
# |--------------------------------------+-------+-----------+-------+--------|
# | Dict{AgentID, Vector{Edge{Main.$T}}} |       |           |       |        |
# | Dict{AgentID, Edge{Main.$T}}         |       |           | x     |        |
# | Dict{AgentID, Vector{AgentID}}       |       | x         |       |        |
# | Dict{AgentID, AgentID}               |       | x         | x     |        |
# | Vector{Vector{Edge{Main.T}}}         | x     |           |       |        |
# | Vector{Edge{Main.T}}                 | x     |           | x     |        |
# | Vector{Vector{AgentID}}              | x     | x         |       |        |
# | Vector{AgentID}                      | x     | x         | x     |        |
# | Dict{AgentID, Vector{Main.$T}}       |       |           |       | x      |
# | Dict{AgentID, Main.$T}               |       |           | x     | x      |
# | Dict{AgentID, EdgeCount}             |       | x         |       | x      |
# | Dict{AgentID, Bool}                  |       | x         | x     | x      |
# | Vector{Vector{Main.$T}}              | x     |           |       | x      |
# | Vector{Main.$T}                      | x     |           | x     | x      |
# | Vector{EdgeCount}                    | x     | x         |       | x      |
# | Vector{Bool}                         | x     | x         | x     | x      |
#
#
# C(B) alone is the type of the container stored per element. We need
# this container type in function as add_agent to construct new container in the
# case that the agent/node doesn't had any incoming edge before. So we
# return also C(B) from the construct_types function.
function construct_types(T, attr::Dict{Symbol, Any})
    ignorefrom = :IgnoreFrom in attr[:props]
    singleedge = :SingleEdge in attr[:props]
    singletype = :SingleAgentType in attr[:props]
    stateless = :Stateless in attr[:props]
    
    A = if singletype
        "Vector{"
    else
        "Dict{AgentID,"
    end

    B = if stateless
        "AgentID"
    elseif ignorefrom
        "Main.$T"
    else
        "Edge{Main.$T}"
    end

    C(B) = if stateless && ignorefrom && singleedge
        "Bool"
    elseif stateless && ignorefrom
        "Int64"
    elseif singleedge
        B
    else
        "Vector{$B}"
    end

    A*C(B)*"}", C(B)
end

function construct_edge_functions(T, attr)
    ignorefrom = :IgnoreFrom in attr[:props]
    singleedge = :SingleEdge in attr[:props]
    singletype = :SingleAgentType in attr[:props]
    stateless = :Stateless in attr[:props]

    singletype_size = get(attr, :size, 0)

    # CT is an abbrev. for ContainerType
    CT = Meta.parse(construct_types(T, attr)[2]) |> eval

    #### Functions that helps to write generic versions of the edge functions
    #
    # _to2idx is used to convert the AgentID to the AgentNr, in the
    # case that the container for the Edges is a Vector (which is the
    # case when the :SingleEdge property is set.
    if singletype
        @eval _to2idx(to::AgentID, ::Type{$(Val{T})}) = agent_nr(to)
    else
        @eval _to2idx(to::AgentID, ::Type{$(Val{T})}) = to
    end

    # _valuetostore is used to retrieve the value that should be stored
    # from an edge, or the (from, edgestate) combination
    if  stateless && ignorefrom && singleedge
        @eval _valuetostore(edge::Edge{Main.$T}) = true
        @eval _valuetostore(from::AgentID, edgestate::Main.$T) = true
    elseif ignorefrom
        @eval _valuetostore(edge::Edge{Main.$T}) = edge.state
        @eval _valuetostore(from::AgentID, edgestate::Main.$T) = edgestate
    elseif stateless
        @eval _valuetostore(edge::Edge{Main.$T}) = edge.from
        @eval _valuetostore(from::AgentID, edgestate::Main.$T) = from
    else
        @eval _valuetostore(edge::Edge{Main.$T}) = edge
        @eval _valuetostore(from::AgentID, edgestate::Main.$T) = Edge(from, edgestate)
    end

    # We must sometime construct the containers, and those can be also
    # a primitivetype when the SingleEdge property is set. To have
    # a uniform construction schema, we define zero methods also for
    # the other cases, and call the constructor in this case
    if !isprimitivetype(CT)
        @eval zero(::Type{$CT}) = $CT()
    end

    @eval _construct_container_func(::Type{$CT}) = () -> zero($CT)
    
    # init_field is used for the :SingleEdge property in combination with a given
    # size to preallocation the vector
    if singletype && singletype_size > 0
        @eval function init_field!(sim, ::Val{Main.$T})
            resize!(sim.$(writefield(T)), $singletype_size)
            for i in 1:$singletype_size
                sim.$(writefield(T))[i] = zero($CT)
            end
        end
    else
        @eval init_field!(_, ::Val{Main.$T}) = nothing
    end

    if singletype && singletype_size == 0
        # for primitivetypes (the Val{true}) we must initialize the new memory
        @eval function _check_size!(field, nr::AgentNr, ::Type{$(Val{T})}, ::Val{true})
            s = size(field, 1)
            if (s < nr)
                resize!(field, nr)
                ccall(:memset, Nothing, (Ptr{Int64}, Int8, Int64),
                      pointer(field, s+1), 0, (nr-s) * sizeof($CT))
            end
        end
        if singleedge 
            @eval function _check_size!(field, nr::AgentNr, ::Type{$(Val{T})}, ::Val{false})
                # resize!(field, nr)
DAS GEHT SO NICHT, DA IM ETI FALL NICHT ERKANNT WERDEN KANN, WELCHE WIEDER RAUSGEFILTERT WERDEN MÜSSEN                
                s = size(field, 1)
                if (s < nr)
                    resize!(field, nr)
                    ccall(:memset, Nothing, (Ptr{Int64}, Int8, Int64),
                          pointer(field, s+1), 0, (nr-s) * sizeof($CT))
                end
            end
        else 
            @eval function _check_size!(field, nr::AgentNr, ::Type{$(Val{T})}, ::Val{false})
                resize!(field, nr)
            end
        end
        @eval function _check_assigned!(field, nr::AgentNr, ::Type{$(Val{T})})     
            if ! isassigned(field, Int64(nr))
                field[nr] = zero($CT)
            end
        end
    else
        @eval _check_size!(_, _, ::Type{$(Val{T})}, _) = nothing
        @eval _check_assigned!(_, _, ::Type{$(Val{T})}) = nothing
    end

    if singletype
        @eval function _get_agent_container(nr::AgentNr, field, ::Type{$(Val{T})})
            _check_size!(field, nr, $(Val{T}), Val(($(isprimitivetype(CT)))))
            _check_assigned!(field, nr, $(Val{T}))
            field[nr]
        end
    else
        @eval function _get_agent_container(to::AgentID, field, ::Type{$(Val{T})})
            get!(_construct_container_func($CT), field, to)
        end
    end


    #### The exported functions
    if stateless && ignorefrom && !singleedge
        @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
            nr = _to2idx(to, $(Val{T}))
            sim.$(writefield(T))[nr] =
                _get_agent_container(nr, sim.$(writefield(T)), $(Val{T})) + 1
        end

        @eval function add_edge!(sim, from::AgentID, to::AgentID, edgestate::Main.$T)
            nr = _to2idx(to, $(Val{T}))
            sim.$(writefield(T))[nr] =
                _get_agent_container(nr, sim.$(writefield(T)), $(Val{T})) + 1
        end
    elseif singleedge
        @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
            nr = _to2idx(to, $(Val{T}))
            _check_size!(sim.$(writefield(T)), nr, $(Val{T}),
                         Val($(isprimitivetype(CT))))
            sim.$(writefield(T))[nr] = _valuetostore(edge)
        end

        @eval function add_edge!(sim, from::AgentID, to::AgentID, edgestate::Main.$T)
            nr = _to2idx(to, $(Val{T}))
            _check_size!(sim.$(writefield(T)), nr, $(Val{T}),
                         Val($(isprimitivetype(CT))))
            sim.$(writefield(T))[nr] = _valuetostore(from, edgestate)
        end
    else
        @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
            nr = _to2idx(to, $(Val{T}))
            push!(_get_agent_container(nr, sim.$(writefield(T)), $(Val{T})),
                  _valuetostore(edge))
        end

        @eval function add_edge!(sim, from::AgentID, to::AgentID, edgestate::Main.$T)
            nr = _to2idx(to, $(Val{T}))
            push!(_get_agent_container(nr, sim.$(writefield(T)), $(Val{T})),
                  _valuetostore(from, edgestate))
        end
    end

    @eval function edges_to(sim, to::AgentID, ::Val{Main.$T}) 
        get($CT, sim.$(readfield(T)), to)
    end

    @eval function prepare_write!(sim, ::Val{Main.$T})
        sim.$(writefield(T)) = Dict{AgentNr, Vector{Edge{Main.$T}}}()
    end

    @eval function finish_write!(sim, ::Val{Main.$T})
        sim.$(readfield(T)) = sim.$(writefield(T))
    end

    @eval function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
        estates = sim.$(readfield(T)) |>
            values |>
            Iterators.flatten |>
            collect |>
            edgestates
        mapreduce(f, op, estates; kwargs...)
    end

    @eval function num_neighbors(sim, to::AgentID, ::Val{Main.$T})
        if haskey(sim.$(readfield(T)), to)
            length(sim.$(readfield(T))[to])
        else
            0
        end
    end
end    

edgefield_type(T, info) = Meta.parse(construct_types(T, info)[1])

edgefield_constructor(T, info) = Meta.parse(construct_types(T, info)[1] * "()")

# Base.@kwdef struct EdgeFieldFactory 
#     constructor = (T, info) -> Meta.parse(construct_types(T, info)[1] * "()")
#     # Functions:
#     init_field = (T, _) -> @eval init_field!(_, ::Val{Main.$T}) = nothing

#     add_edge = (T, info) -> begin
#         t = Meta.parse(construct_types(T, info)[2]) |> eval
#         @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
#             push!(get!($t, sim.$(writefield(T)), to), edge)
#         end
#     end

#     edges_to = (T, info) -> begin
#         t = Meta.parse(construct_types(T, info)[2]) |> eval
#         @eval function edges_to(sim, to::AgentID, ::Val{Main.$T}) 
#             get($t, sim.$(readfield(T)), to)
#         end
#     end

#     prepare_write = (T, _) -> begin
#         @eval function prepare_write!(sim, ::Val{Main.$T})
#             sim.$(writefield(T)) = Dict{AgentNr, Vector{Edge{Main.$T}}}()
#         end
#     end

#     finish_write = (T, _) -> begin 
#         @eval function finish_write!(sim, ::Val{Main.$T})
#             sim.$(readfield(T)) = sim.$(writefield(T))
#         end
#     end 
#     aggregate = (T, _) -> begin
#         @eval function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
#             estates = sim.$(readfield(T)) |>
#                 values |>
#                 Iterators.flatten |>
#                 collect |>
#                 edgestates
#             mapreduce(f, op, estates; kwargs...)
#         end
#     end 
#     # neighborids
#     # neighborstates
#     num_neighbors = (T, _) -> begin
#         @eval function num_neighbors(sim, to::AgentID, ::Val{Main.$T})
#             if haskey(sim.$(readfield(T)), to)
#                 length(sim.$(readfield(T))[to])
#             else
#                 0
#             end
#         end
#     end
# end

#################### Edge Dict

# eff = EdgeFieldFactory()


#################### Edge Dict

# eff_stateless = EdgeFieldFactory(
#     add_edge = (T, _) -> begin
#         @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
#             push!(get!(Vector{AgentID}, sim.$(writefield(T)), to), edge.from)
#         end
#         @eval function add_edge!(sim, from::AgentID, to::AgentID, ::Type{Main.$T})
#             push!(get!(Vector{AgentID}, sim.$(writefield(T)), to), from)
#         end
#         @eval function add_edge!(sim, from::AgentID, to::AgentID, ::Main.$T)
#             push!(get!(Vector{AgentID}, sim.$(writefield(T)), to), from)
#         end
#     end,

#     edges_to = (T, _) -> begin
#         @eval function edges_to(sim, to::AgentID, ::Val{Main.$T}) 
#             @assert false "edges_to can not be called for Stateless edges, use neighbors instead"
#         end
#         @eval function neighbors(sim, to::AgentID, ::Val{Main.$T}) 
#             get(Vector{AgentID}, sim.$(readfield(T)), to)
#         end
#         @eval neighborstates(sim, to::AgentID, edgetype::Val{Main.$T}, agenttype::Val) = 
#             map(e -> agentstate(sim, e, agenttype), neighbors(sim, id, edgetype))  
#         @eval neighborstates_flexible(sim, id::AgentID, edgetype::Val{Main.$T}) =
#             map(e -> agentstate_flexible(sim, e), neighbors(sim, id, edgetype))  
#     end,

#     prepare_write = (T, _) -> begin
#         @eval function prepare_write!(sim, ::Val{Main.$T})
#             sim.$(writefield(T)) = Dict{AgentNr, Vector{AgentID}}()
#         end
#     end,

#     aggregate = (T, _) -> begin
#         @eval function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
#             @assert false "aggregate can not be called for Stateless edges"
#         end
#     end 

# )


# #################### Edge Dict

# function check_agenttype(sim, to::AgentID, ::Val{T}) where T
#     sim.typeinfos.nodes_id2type[type_nr(to)] ==
#         sim.typeinfos.edges_attr[Symbol(T)][:to_agenttype]
# end

# eff_stateless_vec = EdgeFieldFactory(
#     add_edge = (T, _) -> begin
#         @eval function add_edge!(sim, to::AgentID, edge::Edge{Main.$T})
#             #            @mayassert check_agenttype(sim, to, Val(Main.$T)) AGENTSTATE_MSG 
#             add_edge!(sim, edge.from, to, Main.$T())
#             # nr = agent_nr(to)
#             # resize!(sim.$(writefield(T)), nr)
#             # if ! isassigned(sim.$(readfield(T)), Int64(nr))
#             #     sim.$(writefield(T))[nr] = Vector{AgentID}()
#             # end
#             # push!(sim.$(writefield(T))[nr], edge.from)
#         end
#         @eval function add_edge!(sim, from::AgentID, to::AgentID, ::Main.$T)
#             @mayassert check_agenttype(sim, to, Val(Main.$T)) AGENTSTATE_MSG 

#             nr = agent_nr(to)
#             #            check_size(nr, sim.$(writefield(T)))
#             resize!(sim.$(writefield(T)), nr)
#             if ! isassigned(sim.$(readfield(T)), Int64(nr))
#                 sim.$(writefield(T))[nr] = Vector{AgentID}()
#             end
#             push!(sim.$(writefield(T))[nr], from)
#         end
#     end,

#     edges_to = (T, _) -> begin
#         @eval function edges_to(sim, to::AgentID, ::Val{Main.$T}) 
#             @assert false "edges_to can not be called for Stateless edges, use neighbors instead"
#         end
#         @eval function neighbors(sim, to::AgentID, edgetype::Val{Main.$T})
#             if isdefined(sim.$(readfield(T)), Int64(agent_nr(to)))
#                 sim.$(readfield(T))[agent_nr(to)]
#             else
#                 Vector{AgentID}()
#             end
#             @mayassert check_agenttype(sim, to, edgetype) AGENTSTATE_MSG 

#             get(Vector{AgentID}, sim.$(readfield(T)), agent_nr(to))
#         end
#         @eval function neighbors(sim, to::AgentID, edgetype::Val{Main.$T})
#             if isdefined(sim.$(readfield(T)), Int64(agent_nr(to)))
#                 sim.$(readfield(T))[agent_nr(to)]
#             else
#                 Vector{AgentID}()
#             end
#             @mayassert check_agenttype(sim, to, edgetype) AGENTSTATE_MSG 

#             get(Vector{AgentID}, sim.$(readfield(T)), agent_nr(to))
#         end
#         @eval neighborstates(sim, id::AgentID, edgetype::Val{Main.$T}, agenttype::Val) = 
#             map(e -> agentstate(sim, e, agenttype), neighbors(sim, id, edgetype))  
#         @eval neighborstates_flexible(sim, id::AgentID, edgetype::Val{Main.$T}) =
#             map(e -> agentstate_flexible(sim, e), neighbors(sim, id, edgetype))  
#     end,

#     prepare_write = (T, _) -> begin
#         @eval function prepare_write!(sim, ::Val{Main.$T})
#             sim.$(writefield(T)) = Vector{Vector{AgentID}}()
#         end
#     end,

#     aggregate = (T, _) -> begin
#         @eval function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
#             @assert false "aggregate can not be called for Stateless edges"
#         end
#     end,

#     num_neighbors = (T, _) -> begin
#         @eval function num_neighbors(sim, to::AgentID, edgetype::Val{Main.$T})
#             @mayassert check_agenttype(sim, to, edgetype) AGENTSTATE_MSG 

#             if isassigned(sim.$(readfield(T)), Int64(agent_nr(to)))
#                 length(sim.$(readfield(T))[agent_nr(to)])
#             else
#                 0
#             end
#         end
#     end
# )


#################### EdgeFieldFactory Dict

#effs = Dict(:Dict => eff_dict, :Stateless => eff_stateless, :Vec => eff_stateless_vec)

#eff = eff_dict
