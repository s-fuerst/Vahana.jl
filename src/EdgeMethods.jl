import Base.zero

# In the following comment:
# SType stands for the :SingleType trait
# SEdge stands for the :SingleEdge trait
# and
# Ignore stands for the :IgnoreFrom trait
#
# The overall form of the type for the edgefield is the string: A*C(B)}, where
#
# |                             | A             | B             | C(B)      |
# |-----------------------------+---------------+---------------+-----------|
# | Default                     | Dict{AgentID, | Edge{$T}      | Vector{B} |
# | SType                       | Vector{       |               | B         |
# | Stateless & !Ignore         |               | AgentID       |           |
# | Ignore & !Stateless         |               | $T            |           |
# | Stateless & Ignore & !SEdge |               |               | Int       |
# | Stateless & Ignore & SEdge  |               |               | Bool      |
#
# The following table show all the 16 type variante for the container that can be
# constructed following the rules from the table above
#
# |                                 | SType | Stateless | SEdge | Ignore |
# |---------------------------------+-------+-----------+-------+--------|
# | Dict{AgentID, Vector{Edge{$T}}} |       |           |       |        |
# | Dict{AgentID, Edge{$T}}         |       |           | x     |        |
# | Dict{AgentID, Vector{AgentID}}  |       | x         |       |        |
# | Dict{AgentID, AgentID}          |       | x         | x     |        |
# | Vector{Vector{Edge{T}}}         | x     |           |       |        |
# | Vector{Edge{T}}                 | x     |           | x     |        |
# | Vector{Vector{AgentID}}         | x     | x         |       |        |
# | Vector{AgentID}                 | x     | x         | x     |        |
# | Dict{AgentID, Vector{$T}}       |       |           |       | x      |
# | Dict{AgentID, $T}               |       |           | x     | x      |
# | Dict{AgentID, Int64}            |       | x         |       | x      |
# | Dict{AgentID, Bool}             |       | x         | x     | x      |
# | Vector{Vector{$T}}              | x     |           |       | x      |
# | Vector{$T}                      | x     |           | x     | x      |
# | Vector{Int64}                   | x     | x         |       | x      |
# | Vector{Bool}                    | x     | x         | x     | x      |
#
#
# C(B) alone is the type of the container stored per element. We need
# this container type in function as add_agent to construct new container in the
# case that the agent/node doesn't had any incoming edge before. So we
# return also C(B) from the construct_types function.
function construct_types(T, attr::Dict{Symbol, Any})
    ignorefrom = :IgnoreFrom in attr[:traits]
    singleedge = :SingleEdge in attr[:traits]
    singletype = :SingleType in attr[:traits]
    stateless = :Stateless in attr[:traits]
    
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

    A*C(B)*"}", C(B), B
end

edgefield_type(T, info) = Meta.parse(construct_types(T, info)[1])

edgefield_constructor(T, info) = Meta.parse(construct_types(T, info)[1] * "()")

# storage is used for the edges that must be send to a different rank
# as in the edge we do not store the to-id, we use the Tuple to attach this
# information
function edgestorage_type(T, info) 
    type_strings = construct_types(T, info)
    CE = Meta.parse(type_strings[3]) |> eval
    Vector{Vector{Tuple{AgentID, CE}}}
end

# We have some functions that are do only something when some edge traits
# are set and are in this case specialized for the edgetype. Here we define
# the "fallback" functions for the case that no specialized versions are needed.
_check_size!(_, _, _) = nothing
init_field!(_, _) = nothing

show_second_edge_warning = true

function construct_edge_methods(T::DataType, typeinfos, simsymbol)
    attr = typeinfos.edges_attr[T]
    
    ignorefrom = :IgnoreFrom in attr[:traits]
    singleedge = :SingleEdge in attr[:traits]
    singletype = :SingleType in attr[:traits]
    stateless = :Stateless in attr[:traits]

    singletype_size = get(attr, :size, 0)

    # FT is an abbrev. for FieldType (the type for the all the edges of a edgetype)
    # CT is an abbrev. for ContainerType (the type of the container for the edges
    # of a single agent). CE is an abbrev. for ContainerElement (B in the table
    # above).
    type_strings = construct_types(T, attr)
    FT = Meta.parse(type_strings[1]) |> eval
    CT = Meta.parse(type_strings[2]) |> eval
    CE = Meta.parse(type_strings[3]) |> eval
    # we need this when we send the edges via MPI
    attr[:containerelement] = CE
    MT = startswith(string(T), "Main") ? T : @eval Main.$(Symbol(T))

    # for the singletype case we can access the type of the agent via AT
    AT = if singletype
        attr[:target]
    else
        nothing
    end

    mpiactive = mpi.active
    multinode = mpi.size > mpi.shmsize
    
    construct_mpi_edge_methods(T, typeinfos, simsymbol, CE)
    construct_edges_iter_methods(T, attr, simsymbol, FT)
    #### Functions that helps to write generic versions of the edge functions
    #
    # The traits can be different when different models are create using
    # the same types, therefore we dispatch on $simsymbol 
    #
    # _to2idx is used to convert the AgentID to the AgentNr, in the
    # case that the container for the Edges is a Vector (which is the
    # case when the :SingleEdge trait is set.
    if singletype
        @eval _to2idx(_::$simsymbol, to::AgentID, ::Type{$T}) = agent_nr(to)
    else
        @eval _to2idx(_::$simsymbol, to::AgentID, ::Type{$T}) = to
    end

    # _valuetostore is used to retrieve the value that should be stored
    # from an edge, or the (from, edgestate) combination
    if stateless && ignorefrom && singleedge
        @eval _valuetostore(_::$simsymbol, edge::Edge{$MT}) = true
        @eval _valuetostore(_::$simsymbol, from::AgentID, edgestate::$MT) = true
    elseif ignorefrom
        @eval _valuetostore(_::$simsymbol, edge::Edge{$MT}) = edge.state
        @eval _valuetostore(_::$simsymbol, ::AgentID, edgestate::$MT) = edgestate
    elseif stateless
        @eval _valuetostore(_::$simsymbol, edge::Edge{$MT}) = edge.from
        @eval _valuetostore(_::$simsymbol, from::AgentID, ::$MT) = from
    else
        @eval _valuetostore(_::$simsymbol, edge::Edge{$MT}) = edge
        @eval _valuetostore(_::$simsymbol, from::AgentID, edgestate::$MT) = Edge(from, edgestate)
    end

    # We must sometime construct the (per agent) containers, and those
    # can be also a primitivetype when the SingleEdge trait is
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
    # it is a vector with size information (:SingleType with size
    # keyword), the vector will be resized and the memory will be
    # initialized to zero if necessary (= the container is a bittype).
    #
    # _check_size! checks for vectors with a flexible size if the vector
    # is big enough for the agent that operate on this vector, and increase
    # the size if necessary (for other cases it's again a noop).
    if singletype
        if isbitstype(CT)
            #for bitstype we must initialize the new memory
            if singletype_size == 0
                @eval function _check_size!(field, nr::AgentNr, ::Type{$T})
                    s = length(field)
                    if (s < nr)
                        resize!(field, nr)
                        Base.securezero!(view(field, s+1:nr))
                    end
                end
            else
                @eval function init_field!(sim::$simsymbol, ::Type{$MT})
                    resize!(@edgewrite($T), $singletype_size)
                    Base.securezero!(@edgewrite($T))
                end
                @eval function _check_size!(_, ::AgentNr, ::Type{$T})
                end
            end            
        else
            if singletype_size == 0
                @eval function _check_size!(field, nr::AgentNr, ::Type{$T})
                    s = length(field)
                    if (s < nr)
                        resize!(field, nr)
                    end
                end
            else
                @eval init_field!(sim::$simsymbol, ::Type{$MT}) =
                    resize!(@edgewrite($T), $singletype_size)
            end
        end
    end

    #- _can_add is used to check for some singleedge cases, if it's allowed
    # call add_edge! for the agent with id `to`. This should be only done maximal
    # one time, except for the ignorefrom && stateless case, in which we only
    # support the has_edge function, and as true && true = true, we allow multiple
    # add_edge calls in this case. For the singletype case we can not
    # check if add_edge! was already called reliably.
    @eval function _can_add(sim, field, to::AgentID, value, ::Type{$T})
        @mayassert begin
            T = $T
            ! config.check_readable ||
                ! sim.initialized ||
                $attr[:writeable] == true 
        end """
          $T must be in the `write` argument of the transition function.
        """
        
        if $singleedge && !$singletype && !($ignorefrom && $stateless)
            if haskey(field, to)
                if field[to] === value && ! config.quiet
                    if show_second_edge_warning
                        global show_second_edge_warning = false
                        print("""
    
An edge with agent $to as target was added the second time. Since the value of
the edge (after applying traits like :IgnoreFrom) is identical to that of
the first edge, this can be indented and is allowed. 

This warning is only shown once is a Julia session and can be disabled
by calling suppress_warnings(true) after importing Vahana.

                    """)
                    end
                    true
                else
                    false
                end
            else
                true
            end
        else
            true
        end
    end

    #- _get_agent_container / _get_agent_container! unifies
    # the access to the field, incl. possible needed steps like
    # resizing the underlying vector. The *container version return `nothing`
    # in the case, that there is no container for this agent, the *container!
    # version constructs an empty container and returns this one.
    # The last one should be only used for writing edges like in add_edge! and
    # the first one one for read operations like edges
    if singletype
        @eval function _get_agent_container(sim::$simsymbol,
                                     to::AgentID,
                                     ::Type{$T},
                                     field)
            nr = _to2idx(sim, to, $T)

            @mayassert begin
                t = $T
                @edge($T).readable || ! config.check_readable
            end """
            You try to access an edge of type $(t) but the type is not in
            the `read` argument of the apply! function.
            """
            @mayassert begin
                t = $T
                at = $(attr[:target])
                tnr = type_nr(to)
                sim.typeinfos.nodes_id2type[tnr] == $(attr[:target])
            end """
            The :SingleType trait is set for $t, and the agent type is 
            specified as $(at).
            But the type for the given agent is $(sim.typeinfos.nodes_id2type[tnr]).
            """

            _check_size!(field, nr, $T)
            @inbounds isassigned(field, Int64(nr)) ? field[nr] : nothing
        end
        @eval function _get_agent_container!(sim::$simsymbol,
                                      to::AgentID,
                                      ::Type{$T},
                                      field)
            nr = _to2idx(sim, to, $T)

            @mayassert begin
                t = $T
                at = $(attr[:target])
                tnr = type_nr(to)
                sim.typeinfos.nodes_id2type[tnr] == $(attr[:target])
            end """
            The :SingleType trait is set for $t, and the agent type is 
            specified as $(at).
            But the type for the given agent is $(sim.typeinfos.nodes_id2type[tnr]).
            """

            _check_size!(field, nr, $T)
            if !isassigned(field, Int64(nr))
                field[nr] = zero($CT)
            end
            @inbounds field[nr]
        end
    else
        @eval function _get_agent_container!(sim::$simsymbol,
                                      to::AgentID,
                                      ::Type{$T},
                                      field)
            get!(_construct_container_func($CT), field, to)
        end
        @eval function _get_agent_container(sim::$simsymbol, to::AgentID, ::Type{$T}, field)
            @mayassert begin
                t = $T
                @edge($T).readable || ! config.check_readable
            end """
            You try to access an edge of type $(t) but the type is not in
            the `read` argument of the apply! function.
            """
            get(field, to, nothing)
        end
    end

    #### The exported functions
    #- add_edge!
    if stateless && ignorefrom && !singleedge
        @eval function add_edge!(sim::$simsymbol, to::AgentID, _::Edge{$MT})
            @mayassert _can_add(sim, @edgewrite($T), to, nothing, $T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@storage($T)[process_nr(to) + 1], (to, 1))
            else
                nr = _to2idx(sim, to, $T)
                field = @edgewrite($T)
                @inbounds field[nr] = _get_agent_container!(sim, to, $T,
                                                            field) + 1
            end
        end

        @eval function add_edge!(sim::$simsymbol, _::AgentID, to::AgentID, _::$MT)
            @mayassert _can_add(sim, @edgewrite($T), to, nothing, $T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@storage($T)[process_nr(to) + 1], (to, 1))
            else
                nr = _to2idx(sim, to, $T)
                field = @edgewrite($T)
                @inbounds field[nr] = _get_agent_container!(sim, to, $T,
                                                            field) + 1
            end
        end
    elseif singleedge
        @eval function add_edge!(sim::$simsymbol, to::AgentID, edge::Edge{$MT})
            @mayassert _can_add(sim, @edgewrite($T), to,
                                _valuetostore(sim, edge), $T) """
            An edge has already been added to the agent with the id $to (and the
            edgetype traits containing the :SingleEdge trait).
            """
            if $multinode && ! $ignorefrom && node_nr(edge.from) != mpi.node
                push!(@edge($T).
                    accessible[type_nr(edge.from)][process_nr(edge.from) + 1],
                      edge.from)
            end
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@storage($T)[process_nr(to) + 1],
                      (to, _valuetostore(sim, edge)))
            else
                nr = _to2idx(sim, to, $T)
                field = @edgewrite($T)
                _check_size!(field, nr, $T)
                @inbounds field[nr] = _valuetostore(sim, edge)
            end
        end

        @eval function add_edge!(sim::$simsymbol, from::AgentID,
                          to::AgentID, edgestate::$MT)
            @mayassert _can_add(sim, @edgewrite($T), to, nothing, $T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
            @mayassert _can_add(sim, @edgewrite($T), to,
                                _valuetostore(sim, from, edgestate), $T) """
            An edge has already been added to the agent with the id $to (and the
            edgetype traits containing the :SingleEdge trait).
            """
            if $multinode && ! $ignorefrom && node_nr(from) != mpi.node
                push!(@edge($T).
                    accessible[type_nr(from)][process_nr(from) + 1], from)
            end
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@storage($T)[process_nr(to) + 1],
                      (to, _valuetostore(sim, from, edgestate)))
            else
                nr = _to2idx(sim, to, $T)
                field = @edgewrite($T)
                _check_size!(field, nr, $T)
                @inbounds field[nr] = _valuetostore(sim, from, edgestate)
            end
        end
    else
        @eval function add_edge!(sim::$simsymbol, to::AgentID, edge::Edge{$MT})
            @mayassert _can_add(sim, @edgewrite($T), to, nothing, $T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
            if $multinode && ! $ignorefrom && node_nr(edge.from) != mpi.node
                push!(@edge($T).
                    accessible[type_nr(edge.from)][process_nr(edge.from) + 1],
                      edge.from)
            end
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@storage($T)[process_nr(to) + 1],
                      (to, _valuetostore(sim, edge)))
            else
                push!(_get_agent_container!(sim, to, $T, @edgewrite($T)),
                      _valuetostore(sim, edge))
            end
        end

        @eval function add_edge!(sim::$simsymbol, from::AgentID, to::AgentID,
                          edgestate::$MT)
            @mayassert _can_add(sim, @edgewrite($T), to, nothing, $T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
            if $multinode && ! $ignorefrom && node_nr(from) != mpi.node
                push!(@edge($T).
                    accessible[type_nr(from)][process_nr(from) + 1], from)
            end
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@storage($T)[process_nr(to) + 1],
                      (to, _valuetostore(sim, from, edgestate)))
            else
                push!(_get_agent_container!(sim, to, $T, @edgewrite($T)),
                      _valuetostore(sim, from, edgestate))
            end
        end
    end

    # iterate over all edge containers for the agents on the rank,
    # search for edges where on of the agents in the from vector is in
    # the source position, and remove those edges.
    @eval function _remove_edges_agent_source!(sim::$simsymbol, from::Vector{AgentID}, ::Type{$MT})
        # when we don't know where there edges are come from, we can not
        # remove them
        if $ignorefrom
            return nothing
        end

        # when the edge is stateless, the edge is already the agent id.
        @inline function __is_from(edgeorid)
            if $stateless
                edgeorid in from
            else
                edgeorid.from in from
            end
        end

        function _is_from(died) @inline edgeorid ->
            if $stateless
                edgeorid in died
            else
                edgeorid.from in died
            end
        end

        
        for to in keys(@edgewrite($T))
            if $singletype && (! isassigned(@edgewrite($T), to))
                continue
            end
            # ac stands for agent container
            ac = @edgewrite($T)[to]
            if $singleedge # the container is a single element
                if __is_from(ac)
                    if $singletype
                        @edgewrite($T)[to] = zero($CT)
                    else
                        delete!(@edgewrite($T), to)
                    end
                end
            else  
                deleteat!(ac, findall(__is_from, ac))
                if ! $singletype
                    if length(ac) == 0
                        delete!(@edgewrite($T), to)
                    end
                end
            end
        end        
    end
    
    @eval function init_storage!(sim::$simsymbol, ::Type{$MT})
        @storage($T) = [ Vector{Tuple{AgentID, $CE}}() for _ in 1:mpi.size ]
    end


    # It is important that prepare_read! is called before prepare_write!,
    # as in edges_alltoall! the edges are added via add_edge! to the
    # @edgewrite collection. But between finish_write! and prepare_write!
    # @edgeread == @edgewrite.
    @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, t::Type{$MT})
        if add_existing
            @edgewrite($T) = deepcopy(@edgeread($T))
        else
            @edgewrite($T) = $FT()
            init_field!(sim, t)
            foreach(empty!, @storage($T))
            for tnr in 1:length(sim.typeinfos.nodes_types)
                for i in 1:mpi.size
                    empty!(@edge($T).accessible[tnr][i])
                end
            end
        end
        $attr[:writeable] = true
    end

    # It is important that prepare_read! is called before prepare_write!,
    # (the reasoning behind this is mentioned already above).
    @eval function prepare_read!(sim::$simsymbol, _::Vector{DataType}, ::Type{$MT})
        # finish_init! already transmit the edges, so we check last_change > 0
        if @edge($T).last_transmit <= @edge($T).last_change &&
            @edge($T).last_change > 0

            edges_alltoall!(sim, @storage($T), $T)
            foreach(empty!, @storage($T))
        end

        @edge($T).readable = true
    end

    @eval function finish_read!(sim::$simsymbol, ::Type{$MT})
        @edge($T).readable = false
    end
    
    #- finish_write!
    @eval function finish_write!(sim::$simsymbol, ::Type{$MT})
        @edgeread($T) = @edgewrite($T)
        @edge($T).last_change = sim.num_transitions
        $attr[:writeable] = false
    end

    # Rules for the edge functions:
    # stateless => !mapreduce, !edges, !edgestates
    # singledge => !num_edges
    # ignorefrom => !edges, !edgeids

    #- edges
    if !stateless && !ignorefrom
        @eval function edges(sim::$simsymbol, to::AgentID, ::Type{$MT})
            _get_agent_container(sim, to, $T, @edgeread($T))
        end
    else
        @eval function edges(::$simsymbol, ::AgentID, t::Type{$MT})
            @assert false """
            edges is not defined for the trait combination of $t
            """
        end
    end

    #- edgeids
    if !ignorefrom
        if stateless
            @eval function edgeids(sim::$simsymbol, to::AgentID, ::Type{$MT})
                _get_agent_container(sim, to, $T, @edgeread($T))
            end
        else
            if singleedge
                @eval function edgeids(sim::$simsymbol, to::AgentID, ::Type{$MT})
                    ac = _get_agent_container(sim, to, $T, @edgeread($T))
                    isnothing(ac) ? nothing : ac.from
                end
            else
                @eval function edgeids(sim::$simsymbol, to::AgentID, ::Type{$MT})
                    ac = _get_agent_container(sim, to, $T, @edgeread($T))
                    isnothing(ac) ? nothing : map(e -> e.from, ac)
                end
            end
        end
    else
        @eval function edgeids(::$simsymbol, ::AgentID, t::Type{$MT})
            @assert false """
            edgeids is not defined for the trait combination of $t
            """
        end
    end

    #- neighborstates
    if singleedge
        @eval function neighborstates(sim::$simsymbol, id::AgentID,
                               edgetype::Type{$MT}, agenttype::Type) 
            nid = edgeids(sim, id, edgetype)
            isnothing(nid) ? nothing : agentstate(sim, nid, agenttype) 
        end
    else
        @eval function neighborstates(sim::$simsymbol, id::AgentID,
                               edgetype::Type{$MT}, agenttype::Type) 
            checked(map, edgeids(sim, id, edgetype)) do nid
                agentstate(sim, nid, agenttype)
            end
        end
    end


#- edgestates
if !stateless
    if ignorefrom
        @eval function edgestates(sim::$simsymbol, to::AgentID, ::Type{$MT})
            _get_agent_container(sim, to, $T, @edgeread($T))
        end
    else
        if singleedge
            @eval function edgestates(sim::$simsymbol, to::AgentID, ::Type{$MT})
                ac = _get_agent_container(sim, to, $T, @edgeread($T)) 
                isnothing(ac) ? nothing : ac.state
            end
        else
            @eval function edgestates(sim::$simsymbol, to::AgentID, ::Type{$MT})
                ac = _get_agent_container(sim, to, $T, @edgeread($T)) 
                isnothing(ac) ? nothing : map(e -> e.state, ac)
            end
        end
    end
else
    @eval function edgestates(::$simsymbol, ::AgentID, t::Type{$MT})
        @assert false """
            edgestates is not defined for the trait combination of $t
            """
    end
end

#- num_edges
if !singleedge
    if ignorefrom && stateless
        @eval function num_edges(sim::$simsymbol, to::AgentID, ::Type{$MT})
            n = _get_agent_container(sim, to, $T, @edgeread($T))
            isnothing(n) ? 0 : n
        end
    else
        @eval function num_edges(sim::$simsymbol, to::AgentID, ::Type{$MT})
            ac = _get_agent_container(sim, to, $T, @edgeread($T))
            isnothing(ac) ? 0 : length(ac)
        end
    end
else
    @eval function num_edges(::$simsymbol, ::AgentID, t::Type{$MT})
        @assert false """
            num_edges is not defined for the trait combination of $t
            """
    end
end


#- has_edge
if ignorefrom && stateless
    @eval function has_edge(sim::$simsymbol, to::AgentID, ::Type{$MT})
        ac = _get_agent_container(sim, to, $T, @edgeread($T))
        isnothing(ac) || ac == 0 ? false : true
    end
elseif !singleedge 
    @eval function has_edge(sim::$simsymbol, to::AgentID, t::Type{$MT})
        num_edges(sim,to, t) >= 1
    end
elseif !singletype 
    @eval function has_edge(sim::$simsymbol, to::AgentID, ::Type{$MT})
        haskey(@edgeread($T), to)
    end
else
    @eval function has_edge(::$simsymbol, ::AgentID, t::Type{$MT})
        @assert false """
            has_edge is not defined for the trait combination of $t
            """
    end
end

#- _remove_edges (called from Vahana itself to remove the edges of
#- died agents)
if singletype
    @eval function _remove_edges_agent_traget!(sim::$simsymbol, to::AgentID, ::Type{$MT})
        if type_of(sim, to) == $AT
            nr = agent_nr(to)
            if length(@edgewrite($MT)) >= nr
                @edgewrite($MT)[nr] = zero($CT)
            end
        end 
    end
else
    @eval function _remove_edges_agent_traget!(sim::$simsymbol, to::AgentID, ::Type{$MT})
        if haskey(@edgewrite($MT), to)
            delete!(@edgewrite($MT), to)
        end
    end
end

#- mapreduce incl. helper functions
if singletype
    @eval _removeundef(sim::$simsymbol, ::Type{$MT}) = edges -> begin
        [ @inbounds edges[i] for i=1:length(edges) if isassigned(edges, i)]
    end
else
    @eval _removeundef(sim::$simsymbol, ::Type{$MT}) = edges -> edges
end

if ignorefrom && stateless
    @eval function _num_edges(sim::$simsymbol, ::Type{$MT}, write = false)
        field = write ? @edgewrite($T) : @edgeread($T)
        if length(field) == 0
            return 0
        end
        mapreduce(id -> field[id], +, keys(field))
    end
elseif singleedge
    @eval function _num_edges(sim::$simsymbol, t::Type{$MT}, write = false)
        field = write ? @edgewrite($T) : @edgeread($T)
        # TODO: performance improvement with _countundef?
        length(_removeundef(sim, t)(field))
    end
else
    if !singletype
        @eval function _num_edges(sim::$simsymbol, ::Type{$MT}, write = false)
            field = write ? @edgewrite($T) : @edgeread($T)
            if length(field) == 0
                return 0
            end
            mapreduce(id -> length(field[id]), +, keys(field))
        end
    else
        @eval function _num_edges(sim::$simsymbol, ::Type{$MT}, write = false)
            field = write ? @edgewrite($T) : @edgeread($T)
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
end

#- mapreduce
if ! stateless
    @eval function mapreduce(sim::$simsymbol, f, op, t::Type{$MT}; kwargs...)
        with_logger(sim) do
            @info "<Begin> mapreduce edges" f op edgetype=$T
        end
        emptyval = val4empty(op; kwargs...)

        reduced = emptyval
        for e in edges_iterator(sim, $MT)
            if $ignorefrom
                reduced = op(f(e[2]), reduced)
            else
                reduced = op(f(e[2].state), reduced)
            end
        end
        
        mpiop = get(kwargs, :mpiop, op)
        r = MPI.Allreduce(reduced, mpiop, MPI.COMM_WORLD)

        _log_info(sim, "<End> mapreduce edges")

        r
    end
else
    @eval function mapreduce(::$simsymbol, f, op, t::Type{$MT}; kwargs...)
        @assert false """
            mapreduce is not defined for the trait combination of $t
            """
    end
end

end
