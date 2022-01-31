Base.@kwdef mutable struct PG3
    agent_data_t::Dict{DataType, Dict{T_AgentID, Agent}} =
                Dict{DataType, Dict{T_AgentID, Agent}}()
end

pg3 = PG3()
