export AgentID, AgentNr
export Agent
export agent_id
export TypeID
export type_nr

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

abstract type Agent end

const shift_type = BITS_PROCESS + BITS_AGENTNR

# function createId(sim, type::DataType)::AgentID
#     rank = 1
#     id = sim.id_counter[type] 
#     sim.id_counter[type] = id + 1
    
#     AgentID(sim.type2number[type]) << shift_type +
#         rank << BITS_AGENTNR +
#         id
# end

function agent_id(typeID::TypeID, agent_nr::AgentNr)::AgentID
    rank = 1
    # id = sim.id_counter[type] 
    # sim.id_counter[type] = id + 1
    
    AgentID(typeID) << shift_type +
        rank << BITS_AGENTNR +
        agent_nr
end

function agent_id(typeID::Int64, agent_nr::Int64)::AgentID
    @assert typeID <= typemax(TypeID)
    @assert agent_nr <= typemax(AgentNr)
    agent_id(TypeID(typeID), AgentNr(agent_nr))
end

function type_nr(id::AgentID)::TypeID
    id >> (BITS_PROCESS + BITS_AGENTNR)
end

@assert agent_id(3, 1) |> type_nr == 3
    
