# The overall AgentID is composed of three parts:
#
# - The TypeID by which the AgentType of the agent can be determined.
#
# - The ProcessID, which in the MPI implementation is the rank on which
#   the agent is currently managed.
#
# - The AgentNr which says: "This is the nth agent of type TypeID
#   created on Process ID" (so on different processes there can be
#   different agents with the same AgentNr)
#
# In this implementation these three values are packed into a single
# UInt64, but since the construction of the AgentID and the
# determination of the TypeID, ProcessID and AgentNr from the AgentID
# is always done using the interface defined below, this could also be
# changed to a tuple, for example (and this was also tested during
# development, but the implementation found here had most time a noticeable
# performance advantage and a better memory usage).

export AgentID, AgentNr
export agent_id
export TypeID
export type_nr

export add_agent!, add_agents!
export agentstate, agentstate_flexible

const TypeID = UInt8
const BITS_TYPE = 8
const MAX_TYPES = typemax(TypeID) 

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
    add_agent!(sim, agent::T)::AgentID

Add a single agent of type T to the simulation `sim`.

T must have been previously registered by calling
[`register_agenttype!`](@ref).

`add_agent!` returns a new AgentID, which can be used to create edges
from or to this agent before [`finish_init!`](@ref) is called (in the
case that add_agent! is called in the initialization phase), or before
the transition funcion is finished (in the case that add_agent! is
called in an [`apply_transition!`](@ref) callback). Do not use the ID
for other purposes, they are not guaranteed to be stable.

See also [`add_agents!`](@ref), [`add_edge!`](@ref) and [`add_edges!`](@ref)

"""
function add_agent!(::__MODEL__, agent) end


"""
    add_agents!(sim, agents) -> Vector{AgentID}

Add multiple agents at once to the simulation `sim`.

`agents` can be any iterable set of agents, or an arbitrary number of
agents as arguments. 

The types of the agents must have been previously registered by
calling [`register_agenttype!`](@ref).


`add_agents!` returns a vector of AgentIDs, which can be used to
create edges from or to this agents before [`finish_init!`](@ref) is
called (in the case that add_agents! is called in the initialization
phase), or before the transition funcion is finished (in the case that
add_agents!  is called in an [`apply_transition!`](@ref) callback). Do
not use the ID for other purposes, they are not guaranteed to be stable.

See also [`add_agent!`](@ref), [`register_agenttype!`](@ref),
[`add_edge!`](@ref) and [`add_edges!`](@ref)

"""
function add_agents!(sim, agents) 
    [ add_agent!(sim, a) for a in agents ]
end

function add_agents!(sim, agents...) 
    [ add_agent!(sim, a) for a in agents ]
end

"""
    agentstate(sim, id::AgentID, Type{T}) -> T

Returns the state of an agent of type T.

In the case where the type T is not determinable when writing the code
(e.g., since there is no limit to the edges between agents,
[`edges_to`](@ref) can return agentID of different agent types),
[`agentstate_flexible`](@ref) must be used instead.

Warning: if agentstate is called with a Type{T} that does not match the
type of the agent with `id` and the vahana assertions are disabled via
[`enable_asserts`](@ref), then it is possible that the state of
another agent will be returned.
"""
function agentstate(::__MODEL__, ::AgentID, ::Type{T}) where T end

"""
    agentstate_flexible(sim, id::AgentID)

Returns the state of an agent with the `id`, where the type of the
agent is determined at runtime. If the type is known at compile time,
using [`agentstate`](@ref) is preferable as it improves performance.
"""
agentstate_flexible(sim, id::AgentID) =
     sim.nodes_id2read[type_nr(id)](sim)[agent_nr(id)]
