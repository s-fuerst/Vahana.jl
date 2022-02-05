export AgentID, AgentNr
export Agent
export agentId

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

function createId(sim, type::DataType)::AgentID
    rank = 1
    id = sim.id_counter[type] 
    sim.id_counter[type] = id + 1
    
    AgentID(sim.type2number[type]) << shift_type +
        rank << NumBits_AgentNr +
        id
end

function agentId(typeID::TypeID, agentNr::AgentNr)::AgentID
    rank = 1
    # id = sim.id_counter[type] 
    # sim.id_counter[type] = id + 1
    
    AgentID(typeID) << shift_type +
        rank << NumBits_AgentNr +
        agentNr
end

function agentId(typeID::Int64, agentNr::Int64)::AgentID
    @assert typeID <= typemax(TypeID)
    @assert agentNr <= typemax(AgentNr)
    agentId(TypeID(typeID), AgentNr(agentNr))
end
    
