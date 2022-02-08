export Simulation
export add_agenttype!, add_edgetype!
export add_agents!, add_edge!
export agent_typeid
export get_edges
export finish_init!

const NUM_BUFFERS = 2 
const MAX_TYPES = typemax(TypeID)


Base.@kwdef mutable struct Simulation
    name::String
    params::Union{Tuple, NamedTuple}

    agents = Vector{AgentCollection}(undef, MAX_TYPES)
    agent_typeids = Dict{DataType, TypeID}()

    edges = Array{EdgeCollection}(undef, MAX_TYPES)
    edge_typeids = Dict{DataType, TypeID}()
end

function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    sim = Simulation(name = name,
                     params = params)
    add_edgetype!(sim, StatelessEdge)
    sim
end



function Base.show(io::IO, ::MIME"text/plain", sim::Simulation)
    function show_types(io::IO, typeids, coll, name)
        function num_elements(coll::Vector{AgentCollection}, tnr)
            length(coll[tnr])
        end
        
        function num_elements(coll::Vector{EdgeCollection}, tnr)
            if length(coll[tnr]) > 0
                [ length(v) for (_, v) in coll[tnr] ] |> sum
            else 
                0
            end
        end
        
        len = length(typeids)
        if len == 1 
            printstyled(io, "$name Type: "; color = :cyan)
            (k, v) = first(typeids)
            print(io, "$k (ID: $(typeids[k])) \
                       with $(num_elements(coll, v)) $(name)(s)\n")
        elseif len > 1 
            printstyled(io, "$name Types:"; color = :cyan)
            for (k, v) in typeids
                print(io, "\n\t $k (ID: $(typeids[k])) \
                           with $(num_elements(coll, v)) $(name)(s)")
            end
            println()
        end
    end
    
    printstyled(io, "Simulation Name: ", sim.name, "\n"; color = :blue)
    println(io, "Parameters: ", sim.params)
    show_types(io, sim.agent_typeids, sim.agents, "Agent")
    show_types(io, sim.edge_typeids, sim.edges, "Edge")
end

function add_type!(ids::Dict{DataType, TypeID}, T::DataType)
    type_number = length(ids) + 1
    @assert type_number < typemax(TypeID) "Can not add new type, 
                                maximal number of types already registered"
    push!(ids, T => type_number)
    type_number
end


agent_typeid(sim, type::DataType) = sim.agent_typeids[type]

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where { T <: Agent } 
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that the same type is not added twice
    type_number = add_type!(sim.agent_typeids, T)

    sim.agents[type_number] = BufferedAgentDict{T}()
    type_number
end


function add_edgetype!(sim, ::Type{T}) where { T <: AbstractEdge } 
    type_number = add_type!(sim.edge_typeids, T)

    sim.edges[type_number] = BufferedEdgeDict{T}()
    type_number
    #     push!(sim.edges[sim.next],
    #           T => Dict{AgentID,  Vector{Edge{T}}}())
end

function new_agent_id!(sim, id::TypeID)
    c = sim.agents[id]
    c.id_counter = c.id_counter + 1
    agent_id(id, c.id_counter)
end

function add_agents!(sim::Simulation, agent::T) where { T <: Agent } 
    nr = sim.agent_typeids[T]
    id = new_agent_id!(sim, nr)
    sim.agents[nr][id] = agent
    id
end

function add_agents!(sim::Simulation, agents)
    [ add_agents!(sim, a) for a in agents ]
end

function add_agents!(sim::Simulation, agents...)
    [ add_agents!(sim, a) for a in agents ]
end

add_agents!(f::Function) = sim -> add_agents!(sim, f(sim))


function add_edge!(sim, edge::T) where { T <: AbstractEdge }
    dict = sim.edges[sim.edge_typeids[T]]
    push!(dict, edge)
end

function add_edge!(sim, from::AgentID, to::AgentID, state::T) where { T }
    add_edge!(sim, Edge(from, to, state))
end

function get_edges(sim, typeid::TypeID, agent::AgentID)
    sim.edges[typeid][agent]
end

function get_edges(sim, ::Type{T}, agent::AgentID) where { T <: AbstractEdge }
    get_edges(sim, sim.edge_typeids[T], agent)
end

function finish_init!(sim)
end 
