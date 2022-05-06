Base.@kwdef struct NodeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    init
    add_agent
    agentstate
    prepare_write
    transition
    finish_write
    aggregate
end

AGENTSTATE_MSG = "The id of the agent does not match the given type in agentstate"

#################### Dict
nff_dict = NodeFieldFactory(
    type = (T, _) -> :(Dict{AgentNr, Main.$T}),
    constructor = (T, _) -> :(Dict{AgentNr, Main.$T}()),
    init = (_, _) -> :(),

    add_agent = (T, info) -> begin
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

    prepare_write = (T, _) -> begin
        :(function prepare_write!(sim, ::Val{Main.$T})
              sim.$(writefield(T)) = Dict{AgentNr, Main.$T}()
          end)
    end,
    
    transition = (T, info) -> begin
        @eval typeid = $info.nodes_type2id[Main.$T]
        :(function transition!(sim, func, ::Val{Main.$T})
              read = sim.nodes_id2read[$typeid](sim)
              write = sim.nodes_id2write[$typeid](sim)
              for (agentnr, state) in read
                  agentid = agent_id($typeid, agentnr)
                  maybeadd(write, agentnr, func(state, agentid, sim))
              end 
          end)
    end,

    finish_write = (T, _) -> begin
        :(function finish_write!(sim, ::Val{Main.$T})
              sim.$(readfield(T)) = sim.$(writefield(T))
          end)
    end,

    aggregate = (T, _) -> begin
        :(function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
              mapreduce(f, op, values(sim.$(readfield(T))); kwargs...)
          end)
    end, 
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


    prepare_write = (T, info) -> begin
        :(function prepare_write!(sim, ::Val{Main.$T}) end)
    end,
    
    transition = (T, info) -> begin
        @eval typeid = $info.nodes_type2id[Main.$T]
        :(function transition!(sim, func, ::Val{Main.$T})
              read = sim.nodes_id2read[$typeid](sim)
              write = sim.nodes_id2write[$typeid](sim)
              for (agentnr, state) in enumerate(read)
                  # convert to the correct type as enumerate uses Int64
                  agentnr = AgentNr(agentnr) 
                  agentid = agent_id($typeid, agentnr)
                  maybeadd(write, agentnr, func(state, agentid, sim))
              end 
          end)
    end,

    finish_write = (T, _) -> begin
        :(function finish_write!(sim, ::Val{Main.$T})
              sim.$(readfield(T)) = deepcopy(sim.$(writefield(T)))
          end)
    end,

    aggregate = (T, _) -> begin
        :(function aggregate(sim, ::Val{Main.$T}, f, op; kwargs...)
              mapreduce(f, op, sim.$(readfield(T)); kwargs...)
          end)
    end, 
)

nffs = Dict(:Dict => nff_dict, :Vector => nff_vec)

