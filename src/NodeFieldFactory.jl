Base.@kwdef struct NodeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    # Functions:
    init_field = (T, _) -> @eval init_field!(_, ::Type{$T}) = nothing
    add_agent
    agentstate
    prepare_write = (T, _) -> @eval prepare_write!(_, ::Type{$T}) = nothing
    transition
    finish_write
    aggregate
end

AGENTSTATE_MSG = "The id of the agent does not match the given type"

#################### Dict
nff_dict = NodeFieldFactory(
    type = (T, _) -> :(Dict{AgentNr, $T}),
    constructor = (T, _) -> :(Dict{AgentNr, $T}()),

    add_agent = (T, info) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function add_agent!(sim, agent::$T)
            nextid = sim.$(nextidfield(T))
            sim.$(nextidfield(T)) = nextid + 1
            sim.$(writefield(T))[nextid] = agent
            agent_id($typeid, nextid)
        end 
    end,

    agentstate = (T, _) -> begin
        @eval function agentstate(sim, id::AgentID, ::Type{$T})
            @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[$T] AGENTSTATE_MSG
            sim.$(readfield(T))[agent_nr(id)]
        end
    end,

    prepare_write = (T, _) -> begin
        @eval function prepare_write!(sim, ::Type{$T})
            sim.$(writefield(T)) = Dict{AgentNr, $T}()
        end
    end,
    
    transition = (T, info) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function transition!(sim, func, ::Type{$T})
            read = sim.nodes_id2read[$typeid](sim)
            write = sim.nodes_id2write[$typeid](sim)
            for (agentnr, state) in read
                agentid = agent_id($typeid, agentnr)
                maybeadd(write, agentnr, func(state, agentid, sim))
            end 
        end
    end,

    finish_write = (T, _) -> begin
        @eval function finish_write!(sim, ::Type{$T})
            sim.$(readfield(T)) = sim.$(writefield(T))
        end
    end,

    aggregate = (T, _) -> begin
        @eval function aggregate(sim, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, values(sim.$(readfield(T))); kwargs...)
        end
    end, 
)

#################### Vec
nff_vec = NodeFieldFactory(
    type = (T, _) -> :(Vector{$T}),
    constructor = (T, _) -> :(Vector{$T}()),

    init_field = (T, info) -> begin
        if haskey(info.nodes_attr[T], :size)
            s = info.nodes_attr[T][:size]
            @eval function init_field!(sim, ::Type{$T})
                resize!(sim.$(writefield(T)), $s)
            end
        else
            @eval init_field!(sim, ::Type{$T}) = nothing
        end
    end,
    
    add_agent = (T, info) -> begin
        if haskey(info.nodes_attr[T], :size)
            @eval typeid = $info.nodes_type2id[$T]
            @eval function add_agent!(sim, agent::$T)
                agentid = sim.$(nextidfield(T))
                sim.$(nextidfield(T)) = agentid + 1
                @inbounds sim.$(writefield(T))[agentid] = agent
                agent_id($typeid, agentid)
            end
        else
            @eval typeid = $info.nodes_type2id[$T]
            @eval function add_agent!(sim, agent::$T)
                agentid = sim.$(nextidfield(T))
                sim.$(nextidfield(T)) = agentid + 1
                if agentid > size(sim.$(writefield(T)), 1)
                    resize!(sim.$(writefield(T)), agentid)
                end
                @inbounds sim.$(writefield(T))[agentid] = agent
                agent_id($typeid, agentid)
            end
        end
    end,

    agentstate = (T, _) -> begin
        @eval function agentstate(sim, id::AgentID, ::Type{$T})
            @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[$T] AGENTSTATE_MSG 
            @inbounds sim.$(readfield(T))[agent_nr(id)]
        end
    end,

    transition = (T, info) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function transition!(sim, func, ::Type{$T})
            read = sim.nodes_id2read[$typeid](sim)
            write = sim.nodes_id2write[$typeid](sim)
            for (agentnr, state) in enumerate(read)
                # convert to the correct type as enumerate uses Int64
                agentnr = AgentNr(agentnr) 
                agentid = agent_id($typeid, agentnr)
                maybeadd(write, agentnr, func(state, agentid, sim))
            end 
        end
    end,

    finish_write = (T, _) -> begin
        @eval function finish_write!(sim, ::Type{$T})
            sim.$(readfield(T)) = deepcopy(sim.$(writefield(T)))
        end
    end,

    aggregate = (T, _) -> begin
        @eval function aggregate(sim, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, sim.$(readfield(T)); kwargs...)
        end
    end, 
)

nffs = Dict(:Dict => nff_dict, :Vector => nff_vec)

