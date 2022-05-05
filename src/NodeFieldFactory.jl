Base.@kwdef struct NodeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    init
    prepare_write_node
    add_agent
    agentstate
end

AGENTSTATE_MSG = "The id of the agent does not match the given type in agentstate"

#################### Dict
nff_dict = NodeFieldFactory(
    type = (T, _) -> :(Dict{AgentNr, Main.$T}),
    constructor = (T, _) -> :(Dict{AgentNr, Main.$T}()),
    init = (_, _) -> :(),

    prepare_write_node = (T, _) ->
        :(function prepare_write_node!(sim, ::Val{Symbol(Main.$T)})
              sim.$(writefield(T)) = Dict{AgentNr, Main.$T}()
          end),
    
    add_agent = (T, info) ->
        begin
            @eval typeid = $info.nodes_type2id[Main.$T]
            :(function add_agent!(sim, agent::Main.$T)
                  nextid = sim.$(nextidfield(T))
                  sim.$(nextidfield(T)) = nextid + 1
                  sim.$(writefield(T))[nextid] = agent
                  agent_id($typeid, nextid)
              end)
        end,

    agentstate = (T, _) ->
        :(function agentstate(sim, id::AgentID, ::Val{Main.$T})
              @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[Main.$T] AGENTSTATE_MSG
              sim.$(readfield(T))[agent_nr(id)]
          end),
)

function _add_node_vec!(coll, id, node::T) where T
    resize!(coll, id)
    push!(coll[id], node)
end

#################### Vec
nff_vec = NodeFieldFactory(
    type = (T, _) -> :(Vector{Main.$T}),
    constructor = (T, _) -> :(Vector{Main.$T}()),
    init = (T, info) -> 
        if haskey(info.nodes_attr[T], :size)
            s = info.nodes_attr[T][:size]
            :(function init_type!(sim, ::Val{Symbol(Main.$T)})
                  resize!(sim.$(writefield(T)), $s)
              end)
        else
            :()
        end,

    prepare_write_node = (T, info) ->
        if haskey(info.nodes_attr[T], :size)
            s = info.nodes_attr[T][:size]
            :(function prepare_write_node!(sim, ::Val{Symbol(Main.$T)})
                  sim.$(writefield(T)) = Vector{Main.$T}()
                  resize!(sim.$(writefield(T)), $s)
              end)
        else
            :(function prepare_write_node!(sim, ::Val{Main.$T})
                  sim.$(writefield(T)) = Vector{Main.$T}()
              end)
        end,
    
    add_agent = (T, info) ->
        if haskey(info.nodes_attr[T], :size)
            @eval typeid = $info.nodes_type2id[Main.$T]
            :(function add_agent!(sim, agent::Main.$T)
                  agentid = sim.$(nextidfield(T))
                  sim.$(nextidfield(T)) = agentid + 1
                  @inbounds sim.$(writefield(T))[agentid] = agent
                  agent_id($typeid, agentid)
              end)
        else
            @eval typeid = $info.nodes_type2id[Main.$T]
            :(function add_agent!(sim, agent::Main.$T)
                  agentid = sim.$(nextidfield(T))
                  sim.$(nextidfield(T)) = agentid + 1
                  if agentid > size(sim.$(writefield(T)), 1)
                      resize!(sim.$(writefield(T)), agentid)
                  end
                  @inbounds sim.$(writefield(T))[agentid] = agent
                  agent_id($typeid, agentid)
              end)
        end,

    agentstate = (T, _) ->
        :(function agentstate(sim, id::AgentID, ::Val{Main.$T})
              @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[Main.$T] AGENTSTATE_MSG
              @inbounds sim.$(readfield(T))[agent_nr(id)]
          end),
)

nffs = Dict(:Dict => nff_dict, :Vector => nff_vec)

