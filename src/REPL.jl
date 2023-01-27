export num_edges
export show_network, show_agents, show_agent, do_edges

using Printf

######################################## <: EdgeState

function Base.show(io::IO, mime::MIME"text/plain", edge::Edge{T}) where {T}
    show(io, mime, edge.from)
    if fieldnames(T) != ()
        print(io, ": ")
        show(io, mime, edge.state)
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
                           with $(_show_num_agents(sim, t)) agent(s)")
            end
        end

        function show_edge_types(io::IO, sim)
            edges_types = sim.typeinfos.edges_types
            if length(edges_types) >= 1
                printstyled(io, "\nNetworks(s):"; color = :cyan)
            end
            for t in edges_types
                edgetypetraits = sim.typeinfos.edges_attr[t][:traits]
                if (:SingleEdge in edgetypetraits &&
                    :SingleAgentType in edgetypetraits) ||
                    (:SingleAgentType in edgetypetraits &&
                    :size in keys(sim.typeinfos.edges_attr[t]))
                    print(io, "\n\t Type $t with edge(s) for $(_show_num_edges(sim, t)) agent(s)") 
                else
                    print(io, "\n\t Type $t \
                               with $(_show_num_edges(sim, t)) edge(s)")
                    if ! (:SingleAgentType in edgetypetraits)
                        print(io, " for $(_show_num_a_with_e(sim, t)) agent(s)")
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
        
        printstyled(io, "Model Name: ", sim.model.name; color = :magenta)
        if sim.model.name != sim.name
            printstyled(io, "\nSimulation Name: ", sim.name; color = :magenta)
        end
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


function _show_collection(iter, max)
    count = 1
    for (k, v) in iter
        _printid(k)
        print(" => ")
        println(v)
        count += 1
        if count > max
            break
        end
    end
    if count > max
        println("...")
    end
end

# In the :IgnoreFrom case we set edge.from to 0.
function _reconstruct_edge(e, edgetypetraits, edgeT)
    if :Stateless in edgetypetraits
        if :IgnoreFrom in edgetypetraits
            Edge(AgentID(0), edgeT())
        else
            Edge(e, edgeT())
        end
    elseif :IgnoreFrom in edgetypetraits
        Edge(AgentID(0), e)
    else
        e
    end
end        

function _show_edge(sim, e, edgetypetraits, stateof, edgeT)
    e = _reconstruct_edge(e, edgetypetraits, edgeT)
    if !(:IgnoreFrom in edgetypetraits) && e.from == 0
        return
    end
    print("\n\t")
    if :IgnoreFrom in edgetypetraits
        print("unknown           ")
    else
        _printid(e.from)
    end
    if stateof == :Edge || :IgnoreFrom in edgetypetraits
        print(" $(e.state)")
    else
        # for the to edges we get an empty edgetype traits, as we constructed
        # those edges they don't have the same traits
        # But here we need the original traits, so we access them directly
        if :SingleAgentType in sim.typeinfos.edges_attr[edgeT][:traits]
            agentT = sim.typeinfos.edges_attr[edgeT][:to_agenttype]
            aid = agent_id(sim.typeinfos.nodes_type2id[agentT], agent_nr(e.from))
            # We disable assertions to call agentstate from the REPL
            # but this is only done temporary and in a newer world age
            print(" $(Base.invokelatest(agentstate, sim, aid, agentT))")
        else
            print(" $(Base.invokelatest(agentstate_flexible, sim, e.from))")
        end
    end
end


######################################## 

function _show_num_a_with_e(sim, ::Type{T}) where T
    if !sim.initialized 
        "$(length(edgeread(sim, T)))/$(length(edgewrite(sim, T))) (R/W)"
    else
        "$(length(edgeread(sim, T)))"
    end
end

function _show_num_agents(sim, t::Type{T}) where {T}
    "$(num_agents(sim, t))"
end


function _show_num_edges(sim, t::Type{T}) where {T}
    if !sim.initialized 
        "$(num_edges(sim, t; write = true))"
    else
        "$(num_edges(sim, t))"
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


function show_type(sim, ::Type{T}, what::Symbol; write = false, max = 5) where T
    if what == :Agent
        readfield = readstate(sim, T)
        writefield = writestate(sim, T)
    else
        readfield = read(sim, T)
        writefield = Vahana.write(sim, T)
    end

    if length(readfield) > 0
        printstyled("Read:\n"; color = :cyan)
        _show_collection(enumerate(readfield), max)
    end

    if write
        if length(writefield) > 0
            printstyled("Write:\n"; color = :cyan)
            _show_collection(writefield, max)
        end
    end
end

"""
    show_network(sim, ::Type{T}; write = false, max = 5) 

Display (some of) the edges of type T.

In a parallized simulation, only the edges that are in the partition
of the graph associated with the function calling process are shown.

When the keyword `write` is set to true, also the edges that are added in
the initialization phase or current transition function are shown.

The keyword `max` determine the maximal number of shown edges.
"""
show_network(sim, t::Type{T}; kwargs...) where T =
    show_type(sim, t, :Edge; kwargs...)

"""
    show_agents(sim, ::Type{T}) 

Display (some of) the agents of the type T.

In a parallized simulation, only the agents that are in the partition
of the graph associated with the function calling process are shown.

When the keyword `write` is set to true, also the agents that are added in
the initialization phase or current transition function are shown.

The keyword `max` determine the maximal number of shown agents.

"""
show_agents(sim, t::Type{T}; kwargs...) where T =
    show_type(sim, t, :Agent; kwargs...)


"""
    show_agent(sim, Type{T}, id=0; max=5, stateof=:Edge, source = true) 

Display detailed information about the agent with ID `id`, or in the
case that id is a value < 2^32, the information of the nth agent of
type T created. If `id` is 0 (the default value), a random agent of
type T is selected.

Returns the ID of the agent (which is especially useful when a random
agent is selected).

Keyword arguments:

`max` controls the maximal number of edges that are shown per network
(per direction).

`stateof` controls whether the state of the edge (the default) or the
state of the adjacent agent (for any value except :Edge) is displayed.

`source` controls whether all edges of the simulation should be
traversed to find the edges where the agent `id` is the source. Since
this can take some time for large graphs, this search can be disabled.
"""
function show_agent(sim,
             t::Type{T},
             id = 0;
             max = 5,
             stateof = :Edge,
             source = true) where T
    if !sim.initialized
        println("show_agent can not be called before finish_init!.")
        return
    end

    # find a random agent if no id is given (and no agent will ever have id 0
    if id == 0
        agents = readstate(sim, T) |> keys
        
        if ! has_trait(sim, T, :Immortal, :Agent)
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
    if id <= 2 ^ BITS_AGENTNR
        id = agent_id(typeid, AgentNr(id))
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
    printstyled("\nNetwork(s):"; color = :cyan)
    for edgeT in sim.typeinfos.edges_types
        edgeTheadershown = false
        read_container = edgeread(sim, edgeT)
        edgetypetraits = sim.typeinfos.edges_attr[edgeT][:traits]
        justcount = :IgnoreFrom in edgetypetraits && :Stateless in edgetypetraits
        
        if (:SingleEdge in edgetypetraits &&
            :SingleAgentType in edgetypetraits &&
            !justcount)
            printstyled("\n\tIt is not possible to give reliable information " *
                "about the edges of the agent when the\n\tcorresponding edgetype " *
                "has the :SingleEdge and :SingleAgentType trait combination.";
                        color = :red)
            continue
        end
        # The agent_nr(id) returns wrong ids when used for agent of
        # a differnt type then given in :to_agenttype, so we must check this
        if (!(:SingleAgentType in edgetypetraits) ||
            T == sim.typeinfos.edges_attr[edgeT][:to_agenttype])
            # unify the agentid. For vector (:SingleAgent) types, this must
            # be the index, and for dict types, the AgentID
            aid = agent_id(sim.typeinfos.nodes_type2id[T], agent_nr(id))
            nid = :SingleAgentType in edgetypetraits ? agent_nr(id) : aid
            # this rather complex statement checks that the agent has
            # edges of this type towards him. In the :SingleAgentType case we
            # check the vector for #undef entries, in the other case we use haskey
            if (!(:SingleAgentType in edgetypetraits &&
                !isassigned(read_container, nid)) &&
                (:SingleAgentType in edgetypetraits ||
                haskey(read_container, nid)))

                # output the network name and derive some information
                printstyled("\n    $edgeT"; color = :yellow)
                edgeTheadershown = true
                d = read_container[nid]

                if justcount
                    if :SingleEdge in edgetypetraits
                        printstyled("\n\thas_neighbor:  "; color = :green)
                        print("$(has_neighbor(sim, aid, edgeT))")
                    else
                        printstyled("\n\tnum_neighbors: "; color = :green)
                        print("$(num_neighbors(sim, aid, edgeT))")
                    end
                else
                    # write the header for this edgetype, 
                    printstyled("\n\tfrom:              "; color = :green)
                    if stateof == :Edge || :IgnoreFrom in edgetypetraits
                        printstyled("edge.state:"; color = :green)
                    else
                        printstyled("state of edge.from:"; color = :green)
                    end
                    if :SingleEdge in edgetypetraits
                        _show_edge(sim, d, edgetypetraits, stateof, edgeT)
                    else
                        for e in first(d, max)
                            _show_edge(sim, e, edgetypetraits, stateof, edgeT)
                        end
                        if length(d) > max
                            println("\n\t... ($(length(d)-max) not shown)")
                        end
                    end
                end
            end
        end

        if ! source continue end
        
        if :IgnoreFrom in edgetypetraits
            if !justcount
                printstyled("\n\tFor edgetypes with the :IgnoreFrom trait " *
                    "the edges to an agent can not be determined."; color = :red)
            end
            continue
        end
        # collect the outgoing edges and overwrite the from id
        # with the to id
        edges_agents = Vector{Edge}()
        for (eid, e) in edges_iterator(sim, edgeT)
            edge = _reconstruct_edge(e, edgetypetraits, edgeT)
            
            if id == edge.from
                push!(edges_agents, Edge(AgentID(eid), edge.state))
            end   
        end

        if length(edges_agents) > 0
            if ! edgeTheadershown
                printstyled("\n    $edgeT"; color = :yellow)
            end
            printstyled("\n\tto:                "; color = :green)
            if stateof==:Edge
                printstyled("edge.state:"; color = :green)
            else
                printstyled("state of edge.to:"; color = :green)
            end
            for ea in first(edges_agents, max)
                _show_edge(sim, ea, [], stateof, edgeT)
            end
            if length(edges_agents) > max
                println("\n\t... ($(length(edges_agents)-max) not shown)")
            end
        end
    end
    println()

    # set this back to the state at the beginning of the function
    enable_asserts(assert_state)
    
    id
end

"""
TODO DOC 
"""
function num_edges(sim, t::Type{T}; write = false) where T
    _num_edges(sim, t, write)
end

"""
TODO DOC 
"""
function num_agents(sim, ::Type{T}) where T
    field = getproperty(sim, Symbol(T))
    attr = sim.typeinfos.nodes_attr[T]

    if ! (:Immortal in attr[:traits]) || :ConstantSize in attr[:traits]
        died = sim.initialized ? readdied(sim, T) : writedied(sim, T)
        count(i -> ! died[i], 1:field.nextid - 1)
    else
        field.nextid - 1
    end 
end

"""
TODO DOC 
"""
function do_edges(f, h, sim, t::Type{T}) where T
    g = f ∘ (e -> e[2])
    h(g, edges_iterator(sim, t) |> collect)
end

