export num_edges
export show_agent

using Printf

######################################## <: EdgeState

function Base.show(io::IO, mime::MIME"text/plain", edge::Edge{T}) where {T}
    show(io, mime, edge.from)
    if fieldnames(T) != ()
        print(io, ": ")
        show(io, mime, edge.state)
    end
end 

######################################## Model

function Base.show(io::IO, ::MIME"text/plain", model::Model)
    printstyled(io, "\nModel Name: ", model.name; color = :magenta)

    nodes_types = model.types.nodes_types
    if length(nodes_types) >= 1
        printstyled(io, "\nAgent(s):"; color = :cyan)
    end
    for T in nodes_types
        node_hints = model.types.nodes_attr[T][:hints]
        print(io, "\n\t Type $T")
        if :Immortal in node_hints
            printstyled(io, " with Hint: ")
            print(io, ":Immortal")
        end
    end

    edges_types = model.types.edges_types
    if length(edges_types) >= 1
        printstyled(io, "\nEdge(s):"; color = :cyan)
    end
    for T in edges_types
        edge_hints = model.types.edges_attr[T][:hints]
        firsthint = true
        print(io, "\n\t Type $T")
        for k in [ :Stateless, :IgnoreFrom, :SingleEdge, :IgnoreSourceState, :SingleType ]
            if k in edge_hints
                if firsthint
                    firsthint = false
                    printstyled(io, " with Hint(s): ")
                    print(io, "$k")
                else
                    print(io, ", $k")
                end
            end
        end
        if :SingleType in edge_hints
            print(io, "{$(model.types.edges_attr[T][:target])}")
        end
    end
end

######################################## Simulation

function construct_prettyprinting_methods(simsymbol)
    @eval function Base.show(io::IO, ::MIME"text/plain", sim::$simsymbol)
        function show_agent_types(io::IO, sim)
            nodes_types = sim.typeinfos.nodes_types
            if length(nodes_types) >= 1
                printstyled(io, "\nAgent(s):"; color = :cyan)
            end
            for t in nodes_types
                print(io, "\n\t Type $t \
                           with $(num_agents(sim, t)) agent(s)")
                if mpi.active
                    print(io, " ($(num_agents(sim, t, false)) on rank $(mpi.rank))")
                end
            end
        end

        function show_edge_types(io::IO, sim)
            edges_types = sim.typeinfos.edges_types
            if length(edges_types) >= 1
                printstyled(io, "\nEdge(s):"; color = :cyan)
            end
            for t in edges_types
                edgetypehints = sim.typeinfos.edges_attr[t][:hints]
                if (:SingleEdge in edgetypehints &&
                    :SingleType in edgetypehints) ||
                    (:SingleType in edgetypehints &&
                    :size in keys(sim.typeinfos.edges_attr[t]))
                    print(io, "\n\t Type $t with edge(s) for $(num_edges(sim, t)) agent(s)") 
                else
                    print(io, "\n\t Type $t \
                               with $(num_edges(sim, t)) edge(s)")
                    if mpi.active
                        print(io, " ($(num_edges(sim, t, false)) on rank $(mpi.rank))")
                        
                    end
                    if ! (:SingleType in edgetypehints)
                        print(io, " for $(_show_num_a_with_e(sim, t)) agent(s)")
                        if mpi.active
                            print(io, " on this rank")
                        end
                    end
                end
            end
        end

        function show_raster(io::IO, s)
            if length(s.rasters) >= 1 
                printstyled(io, "\nRaster(s):"; color = :cyan)
                for (k,v) in sim.rasters
                    #                    print(io, "\n\t $k: $(getfield(s, k))")
                    print(io, "\n\t :$k with dimension $(size(v))")
                end
            end
        end

        
        function show_struct(io::IO, s, name)
            if nfields(s) >= 1 
                printstyled(io, "\n$(name)(s):"; color = :cyan)
                for k in typeof(s) |> fieldnames
                    f = getfield(s, k)
                    if typeof(f) <: Array
                        if length(f) == 0
                            print(io, "\n\t :$k (empty)")
                        else
                            print(io, "\n\t :$k |> last : $(last(f)) (length: $(length(f)))")
                        end
                        printstyled(io, " "; color = :yellow)
                    else
                        print(io, "\n\t :$k : $f")
                    end
                end
            end
        end
        
        printstyled(io, "\nSimulation Name: ", sim.name; color = :magenta)
        show_struct(io, sim.params, "Parameter")
        show_agent_types(io, sim)
        show_edge_types(io, sim)
        show_raster(io, sim)
        #   print(io, "Globals: ", sim.globals)
        show_struct(io, sim.globals, "Global")
        if ! sim.initialized
            printstyled(io, "\nStill in initialization process!.", color = :red)
        end
    end
end

######################################## Collections

# In the :IgnoreFrom case we set edge.from to 0.
function _reconstruct_edge(sim, e, edgetypehints, edgeT)
    if :Stateless in edgetypehints
        if :IgnoreFrom in edgetypehints
            Edge(AgentID(0), edgeT())
        else
            Edge(e, edgeT())
        end
    elseif :IgnoreFrom in edgetypehints
        Edge(AgentID(0), e)
    else
        e
    end
end        

function _show_edge(sim, e, neighborstate, edgeT)
    function show_struct(s)
        if nfields(s) >= 1
            for k in typeof(s) |> fieldnames
                if neighborstate == true || k in neighborstate
                    f = getfield(s, k)
                    print(" $k=$f,")
                end
            end
        end
    end
    
    
    edgetypehints = sim.typeinfos.edges_attr[edgeT][:hints]
    e = _reconstruct_edge(sim, e, edgetypehints, edgeT)
    if !(:IgnoreFrom in edgetypehints) && e.from == 0
        return
    end
    print("\n\t")
    if :IgnoreFrom in edgetypehints
        print("unknown           ")
    else
        _printid(e.from)
    end
    if ! (:Stateless in edgetypehints)
        print(" $(e.state)")
    end
    if neighborstate == true || length(neighborstate) > 0
        # for the to edges we get an empty edgetype hints, as we constructed
        # those edges they don't have the same hints
        # But here we need the original hints, so we access them directly
        if :SingleType in sim.typeinfos.edges_attr[edgeT][:hints]
            agentT = sim.typeinfos.edges_attr[edgeT][:target]
            aid = agent_id(sim.typeinfos.nodes_type2id[agentT], agent_nr(e.from))
            # We disable assertions to call agentstate from the REPL
            # but this is only done temporary and in a newer world age
            show_struct(Base.invokelatest(agentstate, sim, aid, agentT))
            # print(" $(Base.invokelatest(agentstate, sim, aid, agentT))")
        else
            show_struct(Base.invokelatest(agentstate_flexible, sim, e.from))
            # print(" $(Base.invokelatest(agentstate_flexible, sim, e.from))")
        end
    end
end


######################################## 

function _show_num_a_with_e(sim, ::Type{T}) where T
    if !sim.initialized 
        "$(length(edgewrite(sim, T)))"
    else
        "$(length(edgeread(sim, T)))"
    end
end

# as a convention, we print only "complete" agentid as hex
function _printid(id, nrformatted = true)
    if typeof(id) == UInt32 ||  agent_nr(AgentID(id)) == id
        if nrformatted
            Printf.@printf "%18i" id
        else
            print(id)
        end
    else
        Printf.@printf "0x%016x" id
    end
end    

"""
    show_agent(sim, Type{T}, [id=0; max=5, neighborstate = []]) 

Display detailed information about the agent with ID `id`, or in the
case that id is a value < 2^36, the information of the nth agent of
type T. If `id` is 0 (the default value), a random agent of type T is
selected.

Returns the ID of the agent (which is especially useful when a random
agent is selected).

Keyword arguments:

`max` controls the maximal number of edges that are shown per network
(per direction).

If a field of an agent on the source side of an edge is listed in the
`neighborstate` vector, the value of this field will be also shown.
"""
function show_agent(sim,
             t::Type{T},
             id = 0;
             max = 5,
             neighborstate = []) where T
    if !sim.initialized
        println("show_agent can not be called before finish_init!.")
        return
    end

    # find a random agent if no id is given (and no agent will ever have id 0
    if id == 0
        agents = readstate(sim, T) |> keys
        
        if ! has_hint(sim, T, :Immortal, :Agent)
            field = getproperty(sim, Symbol(T))
            agents = [ a for a in agents if ! field.read.died[a] ]
        end
        
        if length(agents) == 0
            println("No agent of type $T found.")
            return
        end
        id = rand(agents)
    end

    # we want to access the agentstate outside of a transition function,
    # assuming the the id is existing on the node 
    assert_state = asserting()
    enable_asserts(false)
    
    typeid = sim.typeinfos.nodes_type2id[T]
    # id can be always the local nr, so we first ensure that id
    # is the complete agent_id
    id = if id <= 2 ^ BITS_AGENTNR
        agent_id(typeid, AgentNr(id))
    else
        AgentID(id)
    end
    # first we print the id of the agent and the state of the agent
    printstyled("Id / Local Nr: "; color = :cyan)
    _printid(id, false)
    print(" / $(agent_nr(id))")
    # the assert modification has a newer world age
    as = Base.invokelatest(agentstate, sim, id, t)
    if nfields(as) > 0
        printstyled("\nState:"; color = :cyan)
        fnames = as |> typeof |> fieldnames
        for f in fnames
            printstyled("\n    $f=$(getfield(as, f))")
        end
    end
    # the all the edges for the agent
    printstyled("\nEdge(s):"; color = :cyan)
    for edgeT in sim.typeinfos.edges_types
        edgeTheadershown = false
        read_container = edgeread(sim, edgeT)
        edgetypehints = sim.typeinfos.edges_attr[edgeT][:hints]
        justcount = :IgnoreFrom in edgetypehints && :Stateless in edgetypehints
        
        if (:SingleEdge in edgetypehints &&
            :SingleType in edgetypehints &&
            !justcount)
            printstyled("\n\tIt is not possible to give reliable information " *
                "about the edges of the agent when the\n\tcorresponding edgetype " *
                "has the :SingleEdge and :SingleType hint combination.";
                        color = :red)
            continue
        end
        # The agent_nr(id) returns wrong ids when used for agent of
        # a differnt type then given in :target, so we must check this
        if (!(:SingleType in edgetypehints) ||
            T == sim.typeinfos.edges_attr[edgeT][:target])
            # unify the agentid. For vector (:SingleAgent) types, this must
            # be the index, and for dict types, the AgentID
            aid = agent_id(sim.typeinfos.nodes_type2id[T], agent_nr(id))
            nid = :SingleType in edgetypehints ? agent_nr(id) : aid
            # this rather complex statement checks that the agent has
            # edges of this type towards him. In the :SingleType case we
            # check the vector for #undef entries, in the other case we use haskey
            if (!(:SingleType in edgetypehints &&
                !isassigned(read_container, nid)) &&
                (:SingleType in edgetypehints ||
                haskey(read_container, nid)))

                # output the network name and derive some information
                printstyled("\n    $edgeT"; color = :yellow)
                edgeTheadershown = true
                d = read_container[nid]

                if justcount
                    if :SingleEdge in edgetypehints
                        printstyled("\n\thas_edge:  "; color = :green)
                        print("$(has_edge(sim, aid, edgeT))")
                    else
                        printstyled("\n\tnum_edges: "; color = :green)
                        print("$(num_edges(sim, aid, edgeT))")
                    end
                else
                    # write the header for this edgetype, 
                    printstyled("\n\tfrom:              "; color = :green)
                    if ! (:Stateless in edgetypehints)
                        printstyled("edge.state:"; color = :green)
                    elseif neighborstate == true || length(neighborstate) > 0 
                        printstyled("state of edge.from:"; color = :green)
                    end
                    if :SingleEdge in edgetypehints
                        _show_edge(sim, d, neighborstate, edgeT)
                    else
                        for e in first(d, max)
                            _show_edge(sim, e, neighborstate, edgeT)
                        end
                        if length(d) > max
                            println("\n\t... ($(length(d)-max) not shown)")
                        end
                    end
                end
            end
        end
    end
    println()

    # set this back to the state at the beginning of the function
    enable_asserts(assert_state)
    
    id
end

