# The overall AgentID is composed of four parts:
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

export AgentID#, AgentNr, ProcessID
export agent_id

export add_agent!, add_agents!
export agentstate, agentstate_flexible

export num_agents

const TypeID = UInt8
const BITS_TYPE = 8
const MAX_TYPES = 2 ^ BITS_TYPE

const ProcessID = UInt32
const BITS_PROCESS = 18

const AgentNr = UInt64
const BITS_AGENTNR = 38

const AgentID = UInt64

@assert round(log2(typemax(TypeID))) >= BITS_TYPE
@assert round(log2(typemax(ProcessID))) >= BITS_PROCESS
@assert round(log2(typemax(AgentNr))) >= BITS_AGENTNR
@assert round(log2(typemax(AgentID))) >= BITS_TYPE +
    BITS_PROCESS + BITS_AGENTNR 

const SHIFT_TYPE = BITS_PROCESS + BITS_AGENTNR
const SHIFT_RANK = BITS_AGENTNR

function agent_id(typeID::TypeID, agent_nr::AgentNr)::AgentID
    @mayassert typeID <= 2 ^ BITS_TYPE
    @mayassert agent_nr <= 2 ^ BITS_AGENTNR
    AgentID(typeID) << SHIFT_TYPE +
        mpi.rank << SHIFT_RANK +
        agent_nr
end

const process_mask = (2 ^ BITS_PROCESS - 1) << BITS_AGENTNR 
remove_process(agentID::AgentID) = ~process_mask & agentID

# there are other agent_id functions specialized for each AgentType constructed
# via the construct_agent_methods with the signature
# agent_id(sim::$simsymbol, agent_nr, ::Type{$T})

function type_nr(id::AgentID)::TypeID
    id >> SHIFT_TYPE
end

function type_of(sim, id::AgentID)
    sim.typeinfos.nodes_types[type_nr(id)]
end

function process_nr(id::AgentID)::ProcessID
    (id >> SHIFT_RANK) & (2 ^ BITS_PROCESS - 1)
end

function node_nr(id::AgentID)
    fld(process_nr(id), mpi.shmsize)
end

function agent_nr(id::AgentID)::AgentNr
    id & (2 ^ BITS_AGENTNR - 1)
end

@assert agent_id(TypeID(3), AgentNr(1)) |> type_nr == 3
@assert agent_id(TypeID(3), AgentNr(1)) |> agent_nr == 1

"""
    add_agent!(sim, agent::T)::AgentID

Add a single agent of type T to the simulation `sim`.

T must have been previously registered by calling
[`register_agenttype!`](@ref).

`add_agent!` returns a new AgentID, which can be used to create edges
from or to this agent before [`finish_init!`](@ref) is called (in the
case that `add_agent!` is called in the initialization phase), or before
the transition funcion is finished (in the case that add_agent! is
called in an [`apply_transition!`](@ref) callback). Do not use the ID
for other purposes, they are not guaranteed to be stable.

See also [`add_agents!`](@ref), [`add_edge!`](@ref) and [`add_edges!`](@ref)

"""
function add_agent!(::__MODEL__, agent) end


"""
    add_agents!(sim, agents)::Vector{AgentID}

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
    agentstate(sim, id::AgentID, Type{T})::T

Returns the state of an agent of type T.

In the case where the type T is not determinable when writing the code
(e.g., since there is no limit to the edges between agents,
[`edges_to`](@ref) can return agentID of different agent types),
[`agentstate_flexible`](@ref) must be used instead.

!!! warning 

    if agentstate is called with a Type{T} that does not match the
    type of the agent with `id` and the vahana assertions are disabled via
    [`enable_asserts`](@ref), then it is possible that the state of
    an incorrect agent will be returned. When the assertions are active,
    there is a runtime check that the agent with the ID `id` has indeed
    the type T.
"""
function agentstate(::__MODEL__, ::AgentID, ::Type{T}) where T end

"""
    agentstate_flexible(sim, id::AgentID)

Returns the state of an agent with the `id`, where the type of the
agent is determined at runtime. If the type is known at compile time,
using [`agentstate`](@ref) is preferable as it improves performance.
"""
agentstate_flexible(sim, id::AgentID) =
    agentstate(sim, id, sim.typeinfos.nodes_id2type[type_nr(id)])

