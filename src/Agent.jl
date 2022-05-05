export AgentID, AgentNr
export Agent
export agent_id
export TypeID
export type_nr

export add_agent!, add_agents!
export agentstate, agentstate_flexible

const TypeID = UInt8
const BITS_TYPE = 8

const ProcessID = UInt32
const BITS_PROCESS = 24

const AgentNr = UInt32
const BITS_AGENTNR = 32

const AgentID = UInt64

@assert round(log2(typemax(TypeID))) >= BITS_TYPE
@assert round(log2(typemax(ProcessID))) >= BITS_PROCESS
@assert round(log2(typemax(AgentNr))) >= BITS_AGENTNR
@assert round(log2(typemax(AgentID))) >= BITS_TYPE +
    BITS_PROCESS + BITS_AGENTNR 

"""
    abstract type Agent

The different agent types must be concrete subtypes of
~Agent~. These structs define the state of an
edge, but not their (internal) IDs.

See also [`add_agenttype!`](@ref) and [`add_agents!`](@ref)
"""
abstract type Agent end

const shift_type = BITS_PROCESS + BITS_AGENTNR

function agent_id(typeID::TypeID, agent_nr::AgentNr)::AgentID
    rank = 1
    
    AgentID(typeID) << shift_type +
        rank << BITS_AGENTNR +
        agent_nr
end

function agent_id(typeID::Int64, agent_nr::Int64)::AgentID
    @mayassert typeID <= typemax(TypeID)
    @mayassert agent_nr <= typemax(AgentNr)
    agent_id(TypeID(typeID), AgentNr(agent_nr))
end

function type_nr(id::AgentID)::TypeID
    id >> (BITS_PROCESS + BITS_AGENTNR)
end

function process_nr(id::AgentID)::ProcessID
    (id >> (BITS_AGENTNR)) & (2 ^ BITS_PROCESS - 1)
end

function agent_nr(id::AgentID)::AgentNr
    id & (2 ^ BITS_AGENTNR - 1)
end

@assert agent_id(3, 1) |> type_nr == 3


"""
    add_agent!(sim::Simulation, agent::T) -> AgentID

Add a single agent of type T to the simulation `sim`.

T must have been previously registered in the simulation by calling
[`add_agenttype!`](@ref).

`add_agent!` returns a new AgentID, which can be used to create edges
from or to this agent. Do not use the ID for other purposes, they are
not guaranteed to be stable.

See also [`add_agents!`](@ref), [`add_agenttype!`](@ref),
[`add_edge!`](@ref) and [`add_edges!`](@ref)

"""
function add_agent! end

"""
    add_agents!(sim::Simulation, agents) -> Vector{AgentID}

Add multiple agents at once to the simulation `sim`.

`agents` can be any iterable set of agents, or an arbitrary number of
agents as arguments. 

The types of the agents must have been previously registered in the
simulation by calling [`add_agenttype!`](@ref).

`add_agents!` returns a vector of AgentIDs, which can be used
to create edges from or to this agents. Do not use the ID for other
purposes, they are not guaranteed to be stable.

See also [`add_agent!`](@ref), [`add_agenttype!`](@ref),
[`add_edge!`](@ref) and [`add_edges!`](@ref)

"""
function add_agents!(sim, agents) 
    [ add_agent!(sim, a) for a in agents ]
end

function add_agents!(sim, agents...) 
    [ add_agent!(sim, a) for a in agents ]
end

"""
    agentstate(sim::Simulation, id::AgentID) -> T<:Agent

Returns the agent with `id`.
"""
function agentstate end

"""
TODO DOC
"""
agentstate_flexible(sim, id::AgentID) =
     sim.nodes_id2read[type_nr(id)](sim)[agent_nr(id)]

function finish_write_node!(sim, t::Symbol)
    c = sim.typeinfos.nodes[t]
    
    if c == :Dict || c == :Vector
        @eval $sim.$(readfield(t)) = $sim.$(writefield(t))
    end
end

finish_write_node!(sim) = t -> finish_write_node!(sim, t)

prepare_write_node!(sim) = t -> prepare_write_node!(sim, Val(t))
