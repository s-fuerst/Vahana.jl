export Simulation
export add_agenttype!, add_edgetype!
export add_agents!, add_edge!
export finish_init!
# Ist Abstractvector wirklich die sinnvolle Basisklasse
# Ich verlange ja vor allem, dass das Table-Interface unterstützt wird.

const MAX_STEPS = 2 

#TODO: umbauen
Base.@kwdef struct IDFactory
    id_counter = Dict{DataType, T_AgentNr}()
    type2number = Dict{DataType, T_TypeNumber}()
    number2type = Vector{DataType}(undef, typemax(T_TypeNumber))
end



Base.@kwdef mutable struct Simulation15
    name::String
    params::Union{Tuple, NamedTuple}
    # Any will be something like Vahala_Agent_Collection
    agents = fill(Dict{DataType, Any}(), MAX_STEPS)
    edges = fill(Dict{DataType, Any}(), MAX_STEPS)
    id_counter = Dict{DataType, T_AgentNr}()
    type2number = Dict{DataType, T_TypeNumber}()
    number2type = Vector{DataType}(undef, typemax(T_TypeNumber))
    current = 1
    next = 1
end

# TOOD: when simulation structure is fixed, maybe make this
# an inner constructor
function Simulation(name::String,
             params::Union{Tuple, NamedTuple})
    sim = Simulation15(name = name,
                       params = params)
    push!(sim.edges[sim.next], StatelessEdge => Vector{StatelessEdge})
    
    #    add_edgetype!(sim, StatelessEdge(::UInt64, ::UInt64))
    sim
end

# TODO: when simulation structure is fixed, add type annotations to sim
# and return value
# TODO: add data structure for type
function add_agenttype!(sim, ::Type{T}) where {T} #where { T <: Agent }
    # TODO: check isbitstype incl. ignore optional
    # TODO: check that field _id exists and has the correct type
    # TODO: was ist mit privaten Daten
    type_number = reduce(max, values(sim.type2number); init = 0) + 1
    # TODO: assert type_number ist klein genug
 #   push!(sim.agents_t, T => Dict{T_AgentID, T}())
    push!(sim.agents[sim.next], T => Dict{T_AgentID, T}())
    push!(sim.id_counter, T => 0)
    push!(sim.type2number, T => type_number)
    sim.number2type[type_number] = T
    type_number
end

function add_edgetype!(sim, ::Type{T}) where { T } 
#    push!(sim.edges_t, T => Vector{T})
    push!(sim.edges[sim.next],
          T => Dict{T_AgentID,  Vector{Edge{T}}}())
end

function add_agents!(sim::Simulation15, agent::T) where { T <: Agent }
    id = agentId(sim.type2number[T], sim.id_counter[T])
    sim.id_counter[T] = sim.id_counter[T] + 1
    sim.agents[sim.next][T][id] = agent
    id
end

function add_agents!(sim::Simulation15, agents)
    [ add_agents!(sim, a) for a in agents ]
end

function add_agents!(sim::Simulation15, agents...)
    [ add_agents!(sim, a) for a in agents ]
end

add_agents!(f::Function) = sim -> add_agents!(sim, f(sim))


function add_edge!(sim, edge::Edge{T}) where { T }
    dict = get!(sim.edges[sim.next][T], edge.to, Vector{Edge{T}}())
    push!(dict, edge)
end

function add_edge!(sim, from::T_AgentID, to::T_AgentID, state::T) where { T }
    add_edge!(sim, Edge(from, to, state))
end

function finish_init!(sim)
    sim.next = 2
    # sim.agents_t = sim.agents_tp1
    for (k, v) in sim.agents[sim.current]
        sim.agents[sim.next][k] = typeof(v)()
    end

    for (k, v) in sim.edges[sim.current]
        sim.edges[sim.next][k] = typeof(v)()
    end


    # sim.edges_t = sim.edges_tp1
    # for (k, v) in sim.edges_tp1
    #     sim.edges_tp1[k] = typeof(v)()
    # end
end 
