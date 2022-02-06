export AgentID, AgentNr
export Agent
export agent_id

const TypeID = UInt8
const NumBits_Type = 8

const ProcessID = UInt32
const NumBits_Process = 24

const AgentNr = UInt32
const NumBits_AgentNr = 32

const AgentID = UInt64

@assert round(log2(typemax(TypeID))) >= NumBits_Type
@assert round(log2(typemax(ProcessID))) >= NumBits_Process
@assert round(log2(typemax(AgentNr))) >= NumBits_AgentNr
@assert round(log2(typemax(AgentID))) >= NumBits_Type +
    NumBits_Process + NumBits_AgentNr 

abstract type Agent end

const shift_type = NumBits_Process + NumBits_AgentNr

# function createId(sim, type::DataType)::AgentID
#     rank = 1
#     id = sim.id_counter[type] 
#     sim.id_counter[type] = id + 1
    
#     AgentID(sim.type2number[type]) << shift_type +
#         rank << NumBits_AgentNr +
#         id
# end

function agent_id(typeID::TypeID, agent_nr::AgentNr)::AgentID
    rank = 1
    # id = sim.id_counter[type] 
    # sim.id_counter[type] = id + 1
    
    AgentID(typeID) << shift_type +
        rank << NumBits_AgentNr +
        agent_nr
end

function agent_id(typeID::Int64, agent_nr::Int64)::AgentID
    @assert typeID <= typemax(TypeID)
    @assert agent_nr <= typemax(AgentNr)
    agent_id(TypeID(typeID), AgentNr(agent_nr))
end
    
