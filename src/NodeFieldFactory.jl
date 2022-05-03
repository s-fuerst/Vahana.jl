Base.@kwdef struct NodeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    init
    add_agent
    agentstate
end

#################### Dict
nff_dict = NodeFieldFactory(
    type = (T, _) -> :(Dict{AgentID, Main.$T}),
    constructor = (T, _) -> :(Dict{AgentID, Main.$T}()),
    init = (_, _) -> :(),

    add_agent = (T, _) ->
        :(function add_agent!(sim::Simulation, agent::Main.$T)
              agentid = sim.$(nextidfield(T))
              sim.$(writefield(T))[agentid] = agent
              sim.$(nextidfield(T)) = agentid + 1
              agent_id(sim.typeinfos.nodes_type2id[Main.$T], agentid)
          end),

    agentstate = (T, _) ->
        :(function agentstate(sim, id::AgentID, ::Val{Main.$T})
              sim.$(readfield(T))[agent_nr(id)]
          end),
)

function _add_node_vec!(coll, id, node::T) where T
    resize!(coll, id)
    push!(coll[id], node)
end

#################### Vec
nff_vec = NodeFieldFactory(
    type = (T, _) -> :(Vector{$T}),
    constructor = (T, _) -> :(Vector{$T}()),
     init = (T, info) -> 
        if haskey(info.nodes_attr[T], :size)
            s = info.nodes_attr[T][:size]
            :(function init_type!(sim::Simulation, ::Val{Symbol($T)})
                  println($T)
                  resize!(sim.$(writefield(T)), $s)
              end)
        else
            :()
        end,

    add_agent = (T, info) ->
        if haskey(info.nodes_attr[T], :size)
            :(function add_agent!(sim::Simulation, agent::$T)
                  agentid = sim.$(nextidfield(T))
                  sim.$(writefield(T))[agentid] = agent
                  sim.$(nextidfield(T)) = agentid + 1
                  agent_id(sim.typeinfos.nodes_type2id[$T], agentid)
              end)
#            nff_dict.add_agent
        else
            :(function add_agent!(sim::Simulation, agent::$T)
                  agentid = sim.$(nextidfield(T))
                  if agentid > size(sim.$(writefield(T)), 1)
                      resize!(sim.$(writefield(T)), agentid)
                  end
                  sim.$(writefield(T))[agentid] = agent
                  sim.$(nextidfield(T)) = agentid + 1
                  agent_id(sim.typeinfos.nodes_type2id[$T], agentid)
              end)
        end,

    agentstate = (T, _) ->
        :(function agentstate(sim, id::AgentID, ::Val{$T})
              sim.$(readfield(T))[agent_nr(id)]
          end),
#    agentstate = nff_dict.agentstate
)

nffs = Dict(:Dict => nff_dict, :Vector => nff_vec)

