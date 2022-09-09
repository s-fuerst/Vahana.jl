Base.@kwdef struct NodeFieldFactory 
    type # Function Symbol -> Expr
    constructor # Function Symbol -> Expr
    # Functions:
    init_field = (T, _, simsymbol) -> @eval init_field!(sim::$simsymbol, ::Type{$T}) = nothing
    add_agent
    agentstate
    prepare_write 
    transition
    finish_write
    aggregate
end

# TODO: add an exists_agent function

AGENTSTATE_MSG = "The id of the agent does not match the given type"

#################### Dict
nff_dict = NodeFieldFactory(
    type = (T, _) -> :(Dict{AgentNr, $T}),
    constructor = (T, _) -> :(Dict{AgentNr, $T}()),

    init_field = (T, info, simsymbol) -> begin
        @eval function init_field!(sim::$simsymbol, ::Type{$T})
            sim.$(readfield(T)) = Dict{AgentNr, $T}()
            sim.$(writefield(T)) = Dict{AgentNr, $T}()
        end
    end,

    add_agent = (T, info, simsymbol) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function add_agent!(sim::$simsymbol, agent::$T)
            nextid = sim.$(nextidfield(T))
            sim.$(nextidfield(T)) = nextid + 1
            sim.$(writefield(T))[nextid] = agent
            push!(sim.$(reusefield(T)), 0)
            agent_id($typeid, Reuse(0), nextid)
        end 
    end,

    agentstate = (T, _, simsymbol) -> begin
        @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
            @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[$T] AGENTSTATE_MSG
            sim.$(readfield(T))[agent_nr(id)]
        end
    end,

    prepare_write = (T, _, simsymbol) -> begin
        @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
            if add_existing
                sim.$(writefield(T)) = deepcopy(sim.$(readfield(T)))
            else
                sim.$(writefield(T)) = Dict{AgentNr, $T}()
            end
        end
    end,
    
    transition = (T, info, simsymbol) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function transition!(sim::$simsymbol, func, ::Type{$T})
            read::Dict{AgentNr, $T} = sim.nodes_id2read[$typeid](sim)
            write::Dict{AgentNr, $T} = sim.nodes_id2write[$typeid](sim)
            for (agentnr::AgentNr, state::$T) in read
                agentid = agent_id(sim, $typeid, agentnr)
                maybeadd(write, agentnr, func(state, agentid, sim))
            end 
        end,
        @eval function transition_edges_only!(sim::$simsymbol, func, ::Type{$T})
            read::Dict{AgentNr, $T} = sim.nodes_id2read[$typeid](sim)
            for (agentnr::AgentNr, state::$T) in read
                agentid = agent_id(sim, $typeid, agentnr)
                func(state, agentid, sim)
            end 
        end
    end,

    finish_write = (T, _, simsymbol) -> begin
        @eval function finish_write!(sim::$simsymbol, ::Type{$T})
            sim.$(readfield(T)) = sim.$(writefield(T))
        end
    end,

    aggregate = (T, _, simsymbol) -> begin
        @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, values(sim.$(readfield(T))); kwargs...)
        end
    end, 
)

#################### Vec
# for Vectors (immortal agents) we set reuse always fix to 0
nff_vec = NodeFieldFactory(
    type = (T, _) -> :(Vector{$T}),
    constructor = (T, _) -> :(Vector{$T}()),

    init_field = (T, info, simsymbol) -> begin
        if haskey(info.nodes_attr[T], :size)
            s = info.nodes_attr[T][:size]
            @eval function init_field!(sim::$simsymbol, ::Type{$T})
                sim.$(readfield(T)) = Vector{$T}()
                sim.$(writefield(T)) = Vector{$T}()
                resize!(sim.$(writefield(T)), $s)
            end
        else
            @eval function init_field!(sim::$simsymbol, ::Type{$T})
                sim.$(readfield(T)) = Vector{$T}()
                sim.$(writefield(T)) = Vector{$T}()
            end
        end
    end,
    
    add_agent = (T, info, simsymbol) -> begin
        if haskey(info.nodes_attr[T], :size)
            @eval typeid = $info.nodes_type2id[$T]
            @eval function add_agent!(sim::$simsymbol, agent::$T)
                agentid = sim.$(nextidfield(T))
                sim.$(nextidfield(T)) = agentid + 1
                push!(sim.$(reusefield(T)), 0)
                @inbounds sim.$(writefield(T))[agentid] = agent
                immortal_agent_id($typeid, agentid)
            end
        else
            @eval typeid = $info.nodes_type2id[$T]
            @eval function add_agent!(sim::$simsymbol, agent::$T)
                agentid = sim.$(nextidfield(T))
                sim.$(nextidfield(T)) = agentid + 1
                if agentid > size(sim.$(writefield(T)), 1)
                    resize!(sim.$(writefield(T)), agentid)
                end
                push!(sim.$(reusefield(T)), 0)
                @inbounds sim.$(writefield(T))[agentid] = agent
                immortal_agent_id($typeid, agentid)
            end
        end
    end,

    agentstate = (T, _, simsymbol) -> begin
        @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
            @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[$T] AGENTSTATE_MSG 
            @inbounds sim.$(readfield(T))[agent_nr(id)]
        end
    end,

    transition = (T, info, simsymbol) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function transition!(sim::$simsymbol, func, ::Type{$T})
            read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
            write::Vector{$T} = sim.nodes_id2write[$typeid](sim)
            # an own counter (with the correct type) is faster then enumerate 
            idx = AgentNr(0)
            for state::$T in read
                idx += AgentNr(1)
                newstate = func(state, immortal_agent_id($typeid, idx), sim)
                @mayassert newstate !== nothing "You can not use Vectors for mortal agents" 
                write[idx] = newstate
            end 
        end
        @eval function transition_edges_only!(sim::$simsymbol, func, ::Type{$T})
            read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
            # an own counter (with the correct type) is faster then enumerate 
            idx = AgentNr(0)
            for state::$T in read
                idx += AgentNr(1)
                func(state, immortal_agent_id($typeid, idx), sim)
            end 
        end
    end,

    prepare_write = (T, _, simsymbol) -> begin
        @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
            sim.$(writefield(T)) = deepcopy(sim.$(readfield(T)))
        end
    end,

    finish_write = (T, _, simsymbol) -> begin
        @eval function finish_write!(sim::$simsymbol, ::Type{$T})
            sim.$(readfield(T)) = sim.$(writefield(T))
        end
    end,
    
    
    # finish_write = (T, _, simsymbol) -> begin
    #     @eval function finish_write!(sim::$simsymbol, ::Type{$T})
    #         sim.$(readfield(T)) = deepcopy(sim.$(writefield(T)))
    #     end
    # end,

    aggregate = (T, _, simsymbol) -> begin
        @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, sim.$(readfield(T)); kwargs...)
        end
    end, 
)

nffs = Dict(:Dict => nff_dict, :Vector => nff_vec)

