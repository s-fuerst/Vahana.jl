import Base.zero

# In the following comment:
# SType stands for the :SingleType hint
# SEdge stands for the :SingleEdge hint
# and
# Ignore stands for the :IgnoreFrom hint
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
    ignorefrom = :IgnoreFrom in attr[:hints]
    singleedge = :SingleEdge in attr[:hints]
    singletype = :SingleType in attr[:hints]
    stateless = :Stateless in attr[:hints]
    
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

# We have some functions that are do only something when some edge hints
# are set and are in this case specialized for the edgetype. Here we define
# the "fallback" functions for the case that no specialized versions are needed.
_check_size!(_, _, _) = nothing
init_field!(_, _) = nothing

show_second_edge_warning = true

function _can_remove_edges(sim, to, T)
    @mayassert sim.initialized == false || sim.intransition """
             You can call remove_edges! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
    @mayassert begin
        ! config.check_readable ||
            ! sim.initialized ||
            edge_attrs(sim, T)[:writeable] == true 
    end """
      Edge of $T can not removed, as $T is not in the `write` argument of the transition function.
      """
    @mayassert begin
        ! config.check_readable ||
            ! sim.initialized ||
            edge_attrs(sim, T)[:add_existing] == true 
    end """
      $T must be in the `add_existing` keyword of the transition function.
      """
end


function construct_edge_methods(T::DataType, typeinfos, simsymbol, immortal)
    attr = typeinfos.edges_attr[T]
    
    ignorefrom = :IgnoreFrom in attr[:hints]
    singleedge = :SingleEdge in attr[:hints]
    singletype = :SingleType in attr[:hints]
    stateless = :Stateless in attr[:hints]

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
    # The hints can be different when different models are create using
    # the same types, therefore we dispatch on $simsymbol 
    #
    # _to2idx is used to convert the AgentID to the AgentNr, in the
    # case that the container for the Edges is a Vector (which is the
    # case when the :SingleEdge hint is set.
    if singletype
        @eval _to2idx(_::$simsymbol, to::AgentID, ::Type{$T}) = agent_nr(to)
    else
        @eval _to2idx(_::$simsymbol, to::AgentID, ::Type{$T}) = to
    end

    # _valuetostore is used to retrieve the value that should be stored
    # from an edge, or the (from, edgestate) combination
    if stateless && ignorefrom && singleedge
        @eval _valuetostore(_::$simsymbol, edge::Edge{$T}) = true
        @eval _valuetostore(_::$simsymbol, from::AgentID, edgestate::$T) = true
    elseif ignorefrom
        @eval _valuetostore(_::$simsymbol, edge::Edge{$T}) = edge.state
        @eval _valuetostore(_::$simsymbol, ::AgentID, edgestate::$T) = edgestate
    elseif stateless
        @eval _valuetostore(_::$simsymbol, edge::Edge{$T}) = edge.from
        @eval _valuetostore(_::$simsymbol, from::AgentID, ::$T) = from
    else
        @eval _valuetostore(_::$simsymbol, edge::Edge{$T}) = edge
        @eval _valuetostore(_::$simsymbol, from::AgentID, edgestate::$T) = Edge(from, edgestate)
    end

    # We must sometime construct the (per agent) containers, and those
    # can be also a primitivetype when the SingleEdge hint is
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
                @eval function init_field!(sim::$simsymbol, ::Type{$T})
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
                @eval init_field!(sim::$simsymbol, ::Type{$T}) =
                    resize!(@edgewrite($T), $singletype_size)
            end
        end
    else
        # in the other case we must also define this method
        # to ensure that we don't call an old implementation
        # of a previous model with different hints
        @eval function _check_size!(_, ::AgentNr, ::Type{$T})
        end
        @eval function init_field!(sim::$simsymbol, ::Type{$T})
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
                edge_attrs(sim, $T)[:writeable] == true 
        end """
          $T must be in the `write` argument of the transition function.
        """
        
        if $singleedge && !$singletype && !($ignorefrom && $stateless)
            if haskey(field, to) && value !== nothing
                if field[to] === value && ! config.quiet
                    if show_second_edge_warning
                        global show_second_edge_warning = false
                        print("""
    
An edge of type $T with agent $to 
as target was added the second time. Since the value of the edge 
(after applying hints like :IgnoreFrom) is identical to that of
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
            The :SingleType hint is set for $t, and the agent type is 
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
            The :SingleType hint is set for $t, and the agent type is 
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

    # when all agenttypes are immortal, there is no need to check
    # ! sim.model.immortal[type_nr(from)] for every add_edge
    if ignorefrom || reduce(&, immortal)
        @eval function _push_agentsontarget(sim, from, to, _::Type{$T})
        end
    else
        @eval function _push_agentsontarget(sim, from, to, _::Type{$T})
            if ! sim.model.immortal[type_nr(from)]
                push!(get!(@agentsontarget($T), from, Vector{AgentID}()), to)
            end
        end
    end
    
    #### The exported functions
    #- add_edge!
    if stateless && ignorefrom && !singleedge
        @eval function add_edge!(sim::$simsymbol, to::AgentID, _::Edge{$T})
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
            nothing
        end

        @eval function add_edge!(sim::$simsymbol, _::AgentID, to::AgentID, _::$T)
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
            nothing
        end
    elseif singleedge
        @eval function add_edge!(sim::$simsymbol, to::AgentID, edge::Edge{$T})
            @mayassert _can_add(sim, @edgewrite($T), to,
                                _valuetostore(sim, edge), $T) """
            An edge has already been added to the agent with the id $to (and the
            edgetype hints containing the :SingleEdge hint).
            """
            _push_agentsontarget(sim, edge.from, to, $T)
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
            nothing
        end

        @eval function add_edge!(sim::$simsymbol, from::AgentID,
                          to::AgentID, edgestate::$T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
            @mayassert _can_add(sim, @edgewrite($T), to,
                                _valuetostore(sim, from, edgestate), $T) """
            An edge has already been added to the agent with the id $to (and the
            edgetype hints containing the :SingleEdge hint).
            """
            _push_agentsontarget(sim, from, to, $T)
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
            nothing
        end
    else
        @eval function add_edge!(sim::$simsymbol, to::AgentID, edge::Edge{$T})
            @mayassert _can_add(sim, @edgewrite($T), to, nothing, $T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
                """
            _push_agentsontarget(sim, edge.from, to, $T)
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
            nothing
        end

        @eval function add_edge!(sim::$simsymbol, from::AgentID, to::AgentID,
                          edgestate::$T)
            @mayassert _can_add(sim, @edgewrite($T), to, nothing, $T)
            @mayassert sim.initialized == false || sim.intransition """
            You can call add_edge! only in the initialization phase (until
            `finish_init!` is called) or within a transition function called by
            `apply`.
            """
            _push_agentsontarget(sim, from, to, $T)
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
            nothing
        end
    end


    #- remove_edges!
    if singletype
        @eval function remove_edges!(sim::$simsymbol, to::AgentID, ::Type{$T})
            _can_remove_edges(sim, to, $T)
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@removeedges($T)[process_nr(to) + 1], (0, to))
            else
                nr = _to2idx(sim, to, $T)
                field = @edgewrite($T)
                _check_size!(field, nr, $T)
                @inbounds field[nr] = zero($CT)
            end
            nothing
        end
    else
        @eval function remove_edges!(sim::$simsymbol, to::AgentID, ::Type{$T})
            _can_remove_edges(sim, to, $T)
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@removeedges($T)[process_nr(to) + 1], (0, to))
            else
                delete!(@edgewrite($T), to)
            end
            nothing
        end
    end

    if ! ignorefrom
        @eval function remove_edges!(sim::$simsymbol, from::AgentID, to::AgentID,
                              ::Type{$T})
            _can_remove_edges(sim, to, $T)
            if $mpiactive && process_nr(to) != mpi.rank
                push!(@removeedges($T)[process_nr(to) + 1], (from, to))
            else
                nr = _to2idx(sim, to, $T)
                if $singleedge 
                    if ($singletype &&
                        length(@edgewrite($T)) >= nr) ||
                        (! $singletype &&
                        haskey(@edgewrite($T), nr))
                        if $stateless && @edgewrite($T)[nr] == from
                            remove_edges!(sim, to, $T)
                        elseif !$stateless && @edgewrite($T)[nr].from == from
                            remove_edges!(sim, to, $T)
                        end
                    end
                else
                    if ($singletype &&
                        length(@edgewrite($T)) >= nr &&
                        isassigned(@edgewrite($T), nr) &&
                        length(@edgewrite($T)[nr]) > 0) ||
                        (! $singletype &&
                        haskey(@edgewrite($T), nr) &&
                        length(@edgewrite($T)[nr]) > 0)
                        
                        if $stateless
                            @edgewrite($T)[nr] = filter(id -> id != from,
                                                        @edgewrite($T)[nr])
                        else
                            @edgewrite($T)[nr] = filter(e -> e.from != from,
                                                        @edgewrite($T)[nr])
                        end
                    end
                end
            end
        end
    else
        @eval function remove_edges!(sim::$simsymbol, from::AgentID, to::AgentID,
                              ::Type{$T})
            @assert false """
            remove_edges! with a source agent is not defined for edgetypes
            with the :IgnoreFrom hint.
            """
        end
    end    
    

    
    # iterate over all edge containers for the agents on the rank,
# search for edges where on of the agents in the from vector is in
# the source position, and remove those edges.
@eval function _remove_edges_agent_source!(sim::$simsymbol,
                                    from::Vector{AgentID}, ::Type{$T})
    # when we don't know where there edges are come from, we can not
    # remove them
    if $ignorefrom
        return false
    end

    network_changed = false

    check_status = config.check_readable
    config.check_readable = false
    for f in from
        if haskey(@agentsontarget($T), f)
            network_changed = true
            for t in @agentsontarget($T)[f]
                remove_edges!(sim, f, t, $T)
            end
            delete!(@agentsontarget($T), f)
        end
    end

    config.check_readable = check_status
    
    network_changed
end

@eval function init_storage!(sim::$simsymbol, ::Type{$T})
    @storage($T) = [ Tuple{AgentID, $CE}[] for _ in 1:mpi.size ]
    @removeedges($T) = [ Tuple{AgentID, AgentID}[] for _ in 1:mpi.size ]
end


@eval function prepare_write!(sim::$simsymbol,
                       read,
                       add_existing::Bool,
                       t::Type{$T})
    if add_existing
        # when we can not read the types that we are writing
        # there is no need to copy the elements            
        if $T in read
            @edgewrite($T) = deepcopy(@edgeread($T))
        else 
            @edgewrite($T) = @edgeread($T)
        end
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
    edge_attrs(sim, $T)[:writeable] = true
    edge_attrs(sim, $T)[:add_existing] = add_existing
end

@eval function prepare_read!(sim::$simsymbol, _::Vector, ::Type{$T})
    @edge($T).readable = true
end

@eval function finish_read!(sim::$simsymbol, ::Type{$T})
    @edge($T).readable = false
end

# Important: When edges are removed because of dying agents,
# it's allowed that the edgetypes are not in the write argument
# of the apply function, and therefore prepare_write and finish_write
# is not called. Thats okay, as $edgeread = $edgewrite, and
# we update the last_change value when a edge was removed,
# but in the case that in the future additional functionallity
# is added to finish_write, keep this in mind
@eval function finish_write!(sim::$simsymbol, ::Type{$T})
    @edgeread($T) = @edgewrite($T)
    @edge($T).last_change = sim.num_transitions
    edge_attrs(sim, $T)[:writeable] = false
end

@eval function transmit_edges!(sim::$simsymbol, ::Type{$T})
    edges_alltoall!(sim, @storage($T), $T)
    foreach(empty!, @storage($T))
end

@eval function transmit_remove_edges!(sim::$simsymbol, ::Type{$T})
    check_status = config.check_readable
    config.check_readable = false
    removeedges_alltoall!(sim, @removeedges($T), $T)
    foreach(empty!, @removeedges($T))
    config.check_readable = check_status
end

# Rules for the edge functions:
# stateless => !mapreduce, !edges, !edgestates
# singledge => !num_edges
# ignorefrom => !edges, !neighborids

#- edges
if !stateless && !ignorefrom
    @eval function edges(sim::$simsymbol, to::AgentID, ::Type{$T})
        _get_agent_container(sim, to, $T, @edgeread($T))
    end
else
    @eval function edges(::$simsymbol, ::AgentID, t::Type{$T})
        @assert false """
            edges is not defined for the hint combination of $t
            """
    end
end

#- neighborids
if !ignorefrom
    if stateless
        @eval function neighborids(sim::$simsymbol, to::AgentID, ::Type{$T})
            _get_agent_container(sim, to, $T, @edgeread($T))
        end
    else
        if singleedge
            @eval function neighborids(sim::$simsymbol, to::AgentID, ::Type{$T})
                ac = _get_agent_container(sim, to, $T, @edgeread($T))
                isnothing(ac) ? nothing : ac.from
            end
        else
            @eval function neighborids(sim::$simsymbol, to::AgentID, ::Type{$T})
                ac = _get_agent_container(sim, to, $T, @edgeread($T))
                isnothing(ac) ? nothing : map(e -> e.from, ac)
            end
        end
    end
else
    @eval function neighborids(::$simsymbol, ::AgentID, t::Type{$T})
        @assert false """
            neighborids is not defined for the hint combination of $t
            """
    end
end

#- neighborids_iter
if singleedge || ignorefrom
    @eval function neighborids_iter(sim::$simsymbol, to::AgentID, ::Type{$T})
        @assert false """
            neighborids_iter is not defined for edgetypes with the 
            :SingleEdge or :IgnoreFrom hint
        """
    end
elseif stateless
    @eval function neighborids_iter(sim::$simsymbol, to::AgentID, ::Type{$T})
        _get_agent_container(sim, to, $T, @edgeread($T))
    end
else
    @eval function neighborids_iter(sim::$simsymbol, to::AgentID, ::Type{$T})
        ac = _get_agent_container(sim, to, $T, @edgeread($T))
        isnothing(ac) ? nothing : (e.from for e in ac)
    end
end


#- neighborstates
if singleedge
    @eval function neighborstates(sim::$simsymbol, id::AgentID,
                           edgetype::Type{$T}, agenttype::Type) 
        nid = neighborids(sim, id, edgetype)
        isnothing(nid) ? nothing : agentstate(sim, nid, agenttype) 
    end
else
    @eval function neighborstates(sim::$simsymbol, id::AgentID,
                           edgetype::Type{$T}, agenttype::Type)
        nids = neighborids(sim, id, edgetype)
        if nids === nothing
            nothing
        else
            map(nid -> agentstate(sim, nid, agenttype), nids)
        end
    end
end

#- neighborstates_iter
if singleedge || ignorefrom
    @eval function neighborstates_iter(sim::$simsymbol, id::AgentID,
                                edgetype::Type{$T}, agenttype::Type) 
        @assert false """
            neighborstates_iter is not defined for edgetypes with the 
            :SingleEdge or :IgnoreFrom hint
        """
    end
else
    @eval function neighborstates_iter(sim::$simsymbol, id::AgentID,
                                edgetype::Type{$T}, agenttype::Type)
        nids = neighborids(sim, id, edgetype)
        if nids === nothing
            nothing
        else
            (agentstate(sim, nid, agenttype) for nid in nids)
        end
    end
end

#- edgestates
if !stateless
    if ignorefrom
        @eval function edgestates(sim::$simsymbol, to::AgentID, ::Type{$T})
            _get_agent_container(sim, to, $T, @edgeread($T))
        end
    else
        if singleedge
            @eval function edgestates(sim::$simsymbol, to::AgentID, ::Type{$T})
                ac = _get_agent_container(sim, to, $T, @edgeread($T)) 
                isnothing(ac) ? nothing : ac.state
            end
        else
            @eval function edgestates(sim::$simsymbol, to::AgentID, ::Type{$T})
                ac = _get_agent_container(sim, to, $T, @edgeread($T)) 
                isnothing(ac) ? nothing : map(e -> e.state, ac)
            end
        end
    end
else
    @eval function edgestates(::$simsymbol, ::AgentID, t::Type{$T})
        @assert false """
            edgestates is not defined for the hint combination of $t
            """
    end
end

#- edgestate_iter
if singleedge || stateless
    @eval function edgestates_iter(sim::$simsymbol, to::AgentID, ::Type{$T})
        @assert false """
            edgestates_iter is not defined for edgetypes with the 
            :SingleEdge or :Stateless hint
        """
    end
elseif ignorefrom
    @eval function edgestates_iter(sim::$simsymbol, to::AgentID, ::Type{$T})
        _get_agent_container(sim, to, $T, @edgeread($T))
    end
else
    @eval function edgestates_iter(sim::$simsymbol, to::AgentID, ::Type{$T})
        ac = _get_agent_container(sim, to, $T, @edgeread($T))
        isnothing(ac) ? nothing : (e.state for e in ac)
    end
end

#- num_edges
if !singleedge
    if ignorefrom && stateless
        @eval function num_edges(sim::$simsymbol, to::AgentID, ::Type{$T})
            n = _get_agent_container(sim, to, $T, @edgeread($T))
            isnothing(n) ? 0 : n
        end
    else
        @eval function num_edges(sim::$simsymbol, to::AgentID, ::Type{$T})
            ac = _get_agent_container(sim, to, $T, @edgeread($T))
            isnothing(ac) ? 0 : length(ac)
        end
    end
else
    @eval function num_edges(::$simsymbol, ::AgentID, t::Type{$T})
        @assert false """
            num_edges is not defined for the hint combination of $t
            """
    end
end


#- has_edge
if ignorefrom && stateless
    @eval function has_edge(sim::$simsymbol, to::AgentID, ::Type{$T})
        ac = _get_agent_container(sim, to, $T, @edgeread($T))
        isnothing(ac) || ac == 0 ? false : true
    end
elseif !singleedge 
    @eval function has_edge(sim::$simsymbol, to::AgentID, t::Type{$T})
        num_edges(sim,to, t) >= 1
    end
elseif !singletype 
    @eval function has_edge(sim::$simsymbol, to::AgentID, ::Type{$T})
        haskey(@edgeread($T), to)
    end
else
    @eval function has_edge(::$simsymbol, ::AgentID, t::Type{$T})
        @assert false """
            has_edge is not defined for the hint combination of $t
            """
    end
end

# _remove_edges (called from Vahana itself to remove the edges of
# died agents). returns true when edges where removed (and the network
# changed)
if singletype
    @eval function _remove_edges_agent_target!(sim::$simsymbol, to::AgentID, ::Type{$T})
        if type_of(sim, to) == $AT
            nr = agent_nr(to)
            if length(@edgewrite($T)) >= nr
                @edgewrite($T)[nr] = zero($CT)
                true
            else
                false
            end
        else
            false
        end
    end
else
    @eval function _remove_edges_agent_target!(sim::$simsymbol, to::AgentID, ::Type{$T})
        if haskey(@edgewrite($T), to)
            delete!(@edgewrite($T), to)
            true
        else
            false
        end
    end
end

#- mapreduce incl. helper functions
if singletype
    @eval _removeundef(sim::$simsymbol, ::Type{$T}) = edges -> begin
        [ @inbounds edges[i] for i=1:length(edges) if isassigned(edges, i)]
    end
else
    @eval _removeundef(sim::$simsymbol, ::Type{$T}) = edges -> edges
end

if ignorefrom && stateless
    @eval function _num_edges(sim::$simsymbol, ::Type{$T}, write = false)
        field = write ? @edgewrite($T) : @edgeread($T)
        if length(field) == 0
            return 0
        end
        mapreduce(id -> field[id], +, keys(field))
    end
elseif singleedge
    @eval function _num_edges(sim::$simsymbol, t::Type{$T}, write = false)
        field = write ? @edgewrite($T) : @edgeread($T)
        # TODO: performance improvement with _countundef?
        length(_removeundef(sim, t)(field))
    end
else
    if !singletype
        @eval function _num_edges(sim::$simsymbol, ::Type{$T}, write = false)
            field = write ? @edgewrite($T) : @edgeread($T)
            if length(field) == 0
                return 0
            end
            mapreduce(id -> length(field[id]), +, keys(field))
        end
    else
        @eval function _num_edges(sim::$simsymbol, ::Type{$T}, write = false)
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
    @eval function mapreduce(sim::$simsymbol, f, op, t::Type{$T}; kwargs...)
        with_logger(sim) do
            @info "<Begin> mapreduce edges" f op edgetype=$T
        end
        emptyval = val4empty(op; kwargs...)

        reduced = emptyval
        for e in edges_iterator(sim, $T)
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
    @eval function mapreduce(::$simsymbol, f, op, t::Type{$T}; kwargs...)
        @assert false """
            mapreduce is not defined for the hint combination of $t
            """
    end
end


###
@eval function all_edges(sim, t::Type{$T}, all_ranks = true) 
    if num_edges(sim, $T) == 0
        return []
    end

    prepare_read!(sim, [], $T)

    # when we use collect on edges_iterator, we loose
    # for whatever reason the type information we need to call join
    c = Tuple{AgentID, $CE}[]

    for e in edges_iterator(sim, $T, sim.initialized)
        push!(c, e)
    end

    finish_read!(sim, $T)
        
    if all_ranks && mpi.active
        join(c)
    else
        c
    end
end


end
