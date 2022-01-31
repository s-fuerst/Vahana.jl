export T_TypeNumber, T_AgentID
export Agent

const T_TypeNumber = UInt8
const NumBits_Type = 8

const T_ProcessID = UInt32
const NumBits_Process = 24

const T_AgentID = UInt64
const NumBits_AgentNr = 32

@assert log2(typemax(T_TypeNumber)) >= NumBits_Type
@assert log2(typemax(T_ProcessID)) >= NumBits_Process
@assert log2(typemax(T_AgentID)) >= NumBits_Type +
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
