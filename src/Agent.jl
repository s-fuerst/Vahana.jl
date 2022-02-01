export T_AgentID, T_AgentNr
export Agent
export agentId

const T_TypeNumber = UInt8
const NumBits_Type = 8

const T_ProcessID = UInt32
const NumBits_Process = 24

const T_AgentNr = UInt32
const NumBits_AgentNr = 32

const T_AgentID = UInt64

@assert round(log2(typemax(T_TypeNumber))) >= NumBits_Type
@assert round(log2(typemax(T_ProcessID))) >= NumBits_Process
@assert round(log2(typemax(T_AgentNr))) >= NumBits_AgentNr
@assert round(log2(typemax(T_AgentID))) >= NumBits_Type +
    NumBits_Process + NumBits_AgentNr 

abstract type Agent end

const shift_type = NumBits_Process + NumBits_AgentNr

function createId(sim, type::DataType)::T_AgentID
    rank = 1
    id = sim.id_counter[type] 
    sim.id_counter[type] = id + 1
    
    T_AgentID(sim.type2number[type]) << shift_type +
        rank << NumBits_AgentNr +
        id
end

function agentId(typeID::T_TypeNumber, agentNr::T_AgentNr)::T_AgentID
    rank = 1
    # id = sim.id_counter[type] 
    # sim.id_counter[type] = id + 1
    
    T_AgentID(typeID) << shift_type +
        rank << NumBits_AgentNr +
        agentNr
end

function agentId(typeID::Int64, agentNr::Int64)::T_AgentID
    @assert typeID <= typemax(T_TypeNumber)
    @assert agentNr <= typemax(T_AgentNr)
    agentId(T_TypeNumber(typeID), T_AgentNr(agentNr))
end
    
function agentId(sim, ::T, agentNr::T_AgentNr)::T_AgentID where { T }
    agentId(sim.type2number[T], agentNr)
end
