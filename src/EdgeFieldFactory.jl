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
# | Dict{AgentID, Int64}                 |       | x         |       | x      |
# | Dict{AgentID, Bool}                  |       | x         | x     | x      |
# | Vector{Vector{Main.$T}}              | x     |           |       | x      |
# | Vector{Main.$T}                      | x     |           | x     | x      |
# | Vector{Int64}                        | x     | x         |       | x      |
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

edgefield_type(T, info) = Meta.parse(construct_types(T, info)[1])

edgefield_constructor(T, info) = Meta.parse(construct_types(T, info)[1] * "()")


# We have some functions that are do only something when some edge properties
# are set and are in this case specialized for the edgetype. Here we define
# the "fallback" functions for the case that no specialized versions are needed.
_check_size!(_, _, _) = nothing
_check_assigned!(_, _, _) = nothing
init_field!(_, _) = nothing
_can_add(_, _, _) = true


function construct_edge_functions(T::Symbol, attr)
    ignorefrom = :IgnoreFrom in attr[:props]
    singleedge = :SingleEdge in attr[:props]
    singletype = :SingleAgentType in attr[:props]
    stateless = :Stateless in attr[:props]

    singletype_size = get(attr, :size, 0)

    # FT is an abbrev. for FieldType (the type for the all the edges of a edgetype)
    # CT is an abbrev. for ContainerType (the type of the container for the edges of a single agent)
    type_strings = construct_types(T, attr)
    FT = Meta.parse(type_strings[1]) |> eval
    CT = Meta.parse(type_strings[2]) |> eval

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
        @eval _valuetostore(::AgentID, edgestate::Main.$T) = edgestate
    elseif stateless
        @eval _valuetostore(edge::Edge{Main.$T}) = edge.from
        @eval _valuetostore(from::AgentID, ::Main.$T) = from
    else
        @eval _valuetostore(edge::Edge{Main.$T}) = edge
        @eval _valuetostore(from::AgentID, edgestate::Main.$T) = Edge(from, edgestate)
    end

    # We must sometime construct the (per agent) containers, and those
    # can be also a primitivetype when the SingleEdge property is
    # set. To have a uniform construction schema, we define zero
    # methods also for the other cases, and call the constructor in this
    # case.
    if !isprimitivetype(CT)
        @eval zero(::Type{$CT}) = $CT()
    end

    @eval _construct_container_func(::Type{$CT}) = () -> zero($CT)

    #- init_field!, _check_size! and _check_assigned!
    #
    # init_field! is (and must be) called, after the field is
    # constructed. If the field is a dict, init_field! is a noop, for
    # it is a vector with size information (:SingleAgentType with size
    # keyword), the vector will be resized and the memory will be
    # initialized to zero if necessary (= the container is a bittype).
    #
    # _check_size! checks for vectors with a flexible size if the vector
    # is big enough for the agent that operate on this vector, and increase
    # the size if necessary (for other cases it's again a noop).
    #
    # _check_assign is used to create an empty container for the agent
    # if it's the first time that an edge is for this agent 
    if singletype
        if isbitstype(CT)
            # for bitstype we must initialize the new memory
            if singletype_size == 0
                @eval function _check_size!(field, nr::AgentNr, ::Type{$(Val{T})})
                    s = size(field, 1)
                    if (s < nr)
                        resize!(field, nr)
                        ccall(:memset, Nothing, (Ptr{Int64}, Int8, Int64),
                              pointer(field, s+1), 0, (nr-s) * sizeof($CT))
                    end
                end
            else 
                @eval function init_field!(sim, ::Val{Main.$T})
                    field = sim.$(writefield(T))
                    resize!(field, $singletype_size)
                    ccall(:memset, Nothing, (Ptr{Int64}, Int8, Int64),
                          pointer(field), 0, $singletype_size * sizeof($CT))
                end
            end            
        else
            if singletype_size == 0
                @eval function _check_size!(field, nr::AgentNr, ::Type{$(Val{T})})
                    s = size(field, 1)
                    if (s < nr)
                        resize!(field, nr)
                    end
                end
            else
                @eval init_field!(sim, ::Val{Main.$T}) =
                    resize!(sim.$(writefield(T)), $singletype_size)
            end
            
            @eval function _check_assigned!(field, nr::AgentNr, ::Type{$(Val{T})})     
                if ! isassigned(field, Int64(nr))
                    field[nr] = zero($CT)
                end
            end
        end
    end

    #- _can_add
    if singleedge && !singletype && !(ignorefrom && stateless)
        @eval function _can_add(field, to::AgentID, ::Type{$(Val{T})})
            !haskey(field, to)
        end
    end

    #- _get_agent_container
    if singletype
        @eval function _get_agent_container(sim, to::AgentID, ::Type{$(Val{T})}, field)
            nr = _to2idx(to, $(Val{T}))

            @mayassert begin
                at = $(attr[:to_agenttype])
                tnr = type_nr(to)
                sim.typeinfos.nodes_id2type[tnr] == $(attr[:to_agenttype])
            end """
            The :SingleAgentType property is set, and the agent type is specified as $(at).
            But the type of the `to` agent is $(sim.typeinfos.nodes_id2type[tnr]).
            """

            _check_size!(field, nr, $(Val{T}))
            _check_assigned!(field, nr, $(Val{T}))
            field[nr]
        end
    else
        @eval function _get_agent_container(sim, to::AgentID, ::Type{$(Val{T})}, field)
            get!(_construct_container_func($CT), field, to)
        end
    end

    #### The exported functions
    #- add_edge!
    if stateless && ignorefrom && !singleedge
        @eval function add_edge!(sim::Simulation, to::AgentID, ::Edge{Main.$T})
            nr = _to2idx(to, $(Val{T}))
            sim.$(writefield(T))[nr] =
                _get_agent_container(sim, to, $(Val{T}), sim.$(writefield(T))) + 1
        end

        @eval function add_edge!(sim::Simulation, ::AgentID, to::AgentID, ::Main.$T)
            nr = _to2idx(to, $(Val{T}))
            sim.$(writefield(T))[nr] =
                _get_agent_container(sim, to, $(Val{T}), sim.$(writefield(T))) + 1
        end
    elseif singleedge
        @eval function add_edge!(sim::Simulation, to::AgentID, edge::Edge{Main.$T})
            @mayassert _can_add(sim.$(writefield(T)), to, $(Val{T})) """
            An edge has already beend added to the agent with the id $to (and the
            edgetype properties containing the :SingleEdge property).
            """
            nr = _to2idx(to, $(Val{T}))
            _check_size!(sim.$(writefield(T)), nr, $(Val{T}))
            sim.$(writefield(T))[nr] = _valuetostore(edge)
        end

        @eval function add_edge!(sim::Simulation, from::AgentID, to::AgentID, edgestate::Main.$T)
            @mayassert _can_add(sim.$(writefield(T)), to, $(Val{T})) """
            An edge has already beend added to the agent with the id $to (and the
            edgetype properties containing the :SingleEdge property).
            """
            nr = _to2idx(to, $(Val{T}))
            _check_size!(sim.$(writefield(T)), nr, $(Val{T}))
            sim.$(writefield(T))[nr] = _valuetostore(from, edgestate)
        end
    else
        @eval function add_edge!(sim::Simulation, to::AgentID, edge::Edge{Main.$T})
            push!(_get_agent_container(sim, to, $(Val{T}), sim.$(writefield(T))),
                  _valuetostore(edge))
        end

        @eval function add_edge!(sim::Simulation, from::AgentID, to::AgentID, edgestate::Main.$T)
            push!(_get_agent_container(sim, to, $(Val{T}), sim.$(writefield(T))),
                  _valuetostore(from, edgestate))
        end
    end

    #- prepare_write! 
    @eval function prepare_write!(sim, t::Val{Main.$T})
        sim.$(writefield(T)) = $FT()
        init_field!(sim, t)
    end

    #- finish_write!
    @eval function finish_write!(sim, ::Val{Main.$T})
        sim.$(readfield(T)) = sim.$(writefield(T))
    end

    # Rules for the edge functions:
    # Stateless => !aggregate, !edges_to, !neighbor_states
    # SEdge => !num_neighbors
    # Ignore => !edges_to, !neighbor_ids

    #- edges_to
    if !stateless && !ignorefrom
        @eval function edges_to(sim::Simulation, to::AgentID, ::Val{Main.$T})
            _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T)))
        end
    else
        @eval function edges_to(::Simulation, ::AgentID, t::Val{Main.$T})
            @assert false """
            edges_to is not defined for the property combination of $t
            """
        end
    end

    #- neighborids
    if !ignorefrom
        if stateless
            @eval function neighborids(sim::Simulation, to::AgentID, ::Val{Main.$T})
                _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T)))
            end
        else
            if singleedge
                @eval function neighborids(sim::Simulation, to::AgentID, ::Val{Main.$T})
                    _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))).from
                end
            else
                @eval function neighborids(sim::Simulation, to::AgentID, ::Val{Main.$T})
                    map(e -> e.from,
                        _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))))
                end
            end
        end
    else
        @eval function neighborids(::Simulation, ::AgentID, t::Val{Main.$T})
            @assert false """
            neighborids is not defined for the property combination of $t
            """
        end
    end

    #- edgestates
    if !stateless
        if ignorefrom
            @eval function edgestates(sim::Simulation, to::AgentID, ::Val{Main.$T})
                _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T)))
            end
        else
            if singleedge
                @eval function edgestates(sim::Simulation, to::AgentID, ::Val{Main.$T})
                    _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))).state
                end
            else
                @eval function edgestates(sim::Simulation, to::AgentID, ::Val{Main.$T})
                    map(e -> e.state,
                        _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))))
                end
            end
        end
    else
        @eval function edgestates(::Simulation, ::AgentID, t::Val{Main.$T})
            @assert false """
            edgestates is not defined for the property combination of $t
            """
        end
    end

    #- num_neighbors
    if !singleedge
        if ignorefrom && stateless
            @eval function num_neighbors(sim::Simulation, to::AgentID, ::Val{Main.$T})
                _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T)))
            end
        else
            @eval function num_neighbors(sim::Simulation, to::AgentID, ::Val{Main.$T})
                size(_get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))),1)
            end
        end
    else
        @eval function num_neighbors(::Simulation, ::AgentID, t::Val{Main.$T})
            @assert false """
            num_neighbors is not defined for the property combination of $t
            """
        end
    end


    #- has_neighbor
    if ignorefrom && stateless
        @eval function has_neighbor(sim::Simulation, to::AgentID, ::Val{Main.$T})
            _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))) >= 1
        end
    elseif !singleedge
        @eval function has_neighbor(sim::Simulation, to::AgentID, t::Val{Main.$T})
            num_neighbors(sim,to, t) >= 1
        end
    elseif !singletype 
        @eval function has_neighbor(sim::Simulation, to::AgentID, ::Val{Main.$T})
            haskey(sim.$(readfield(T)), to)
        end
    else
        @eval function has_neighbor(::Simulation, ::AgentID, t::Val{Main.$T})
            @assert false """
            has_neighbor is not defined for the property combination of $t
            """
        end
    end

    #- _vectorized_container
    # if !singleedge
    #     @eval function _vectorized_container_read(sim::Simulation, to::AgentID, ::Val{Main.$T})
    #         _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))) 
    #     end
    #     @eval function _vectorized_container_write(sim::Simulation, to::AgentID, ::Val{Main.$T})
    #         _get_agent_container(sim, to, $(Val{T}), sim.$(writefield(T))) 
    #     end
    # else
    #     @eval function _vectorized_container_read(sim::Simulation, to::AgentID, ::Val{Main.$T})
    #         [ _get_agent_container(sim, to, $(Val{T}), sim.$(readfield(T))) ]
    #     end
    #     @eval function _vectorized_container_write(sim::Simulation, to::AgentID, ::Val{Main.$T})
    #         [ _get_agent_container(sim, to, $(Val{T}), sim.$(writefield(T))) ]
    #     end
    # end


    if singletype
        @eval _removeundef(::Val{Main.$T}) = edges ->
            [ edges[i] for i=1:length(edges) if isassigned(edges, i)]
    else
        @eval _removeundef(::Val{Main.$T}) = edges -> edges
    end

    if !singleedge
        if !singletype
            @eval function _num_edges(sim::Simulation, t::Val{Main.$T}, write = false)
                field = write ? sim.$(writefield(T)) : sim.$(readfield(T))
                if length(field) == 0
                    return 0
                end
                mapreduce(id -> length(field[id]), +, keys(field))
            end
        else
            @eval function _num_edges(sim::Simulation, t::Val{Main.$T}, write = false)
                field = write ? sim.$(writefield(T)) : sim.$(readfield(T))
                if length(field) == 0
                    return 0
                end
                n = 0
                for i in 1:length(field)
                    if isassigned(field, i)
                        n += length(field[i])
                    end
                end
                n
            end
        end 
    else
        @eval function _num_edges(sim::Simulation, t::Val{Main.$T}, write = false)
            field = write ? sim.$(writefield(T)) : sim.$(readfield(T))
            length(_removeundef(t)(field))
        end
    end
    
    #- aggregate
    if !stateless
        if ignorefrom
            @eval _edgestates(::Val{Main.$T}) = edges -> edges
        else
            @eval _edgestates(::Val{Main.$T}) = edges -> map(e -> e.state, edges)
        end
        if singleedge
            @eval function aggregate(sim::Simulation, t::Val{Main.$T}, f, op; kwargs...)
                estates = sim.$(readfield(T)) |>
                    values |>
                    _removeundef(t) |>
                    collect |>
                    _edgestates(t)
                mapreduce(f, op, estates; kwargs...)
            end
        else
            @eval function aggregate(sim::Simulation, t::Val{Main.$T}, f, op; kwargs...)
                estates = sim.$(readfield(T)) |>
                    values |>
                    _removeundef(t) |>
                    Iterators.flatten |>
                    collect |>
                    _edgestates(t)
                mapreduce(f, op, estates; kwargs...)
            end
        end
    else
        @eval function aggregate(::Simulation, t::Val{Main.$T}, f, op; kwargs...)
            @assert false """
            aggregate is not defined for the property combination of $t
            """
        end
    end
end    


