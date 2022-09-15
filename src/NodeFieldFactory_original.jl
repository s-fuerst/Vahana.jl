



Base.@kwdef struct NodeFieldFactory 
    type = (T, _) -> :(Vector{$T})
    constructor = (T, _) -> :(Vector{$T}())
    # Functions:
    init_field
    add_agent
    agentstate
    prepare_write 
    transition
    finish_write
    aggregate
    register_atexit = (_, _) -> nothing
end

AGENTSTATE_MSG = "The id of the agent does not match the given type"

# TODO: check if we get a noticeable performance improvment, when we
# compile this function for each T
function _get_next_id(sim, T, reusefield, nextidfield, mortal)
    if mortal 
        attrs = sim.typeinfos.nodes_attr[T]
        if length(attrs[:reuseable]) > 0
            nr = pop!(attrs[:reuseable])
            @assert attrs[:died][nr]
            attrs[:died][nr] = false
            reusefield[nr] = reusefield[nr] + 1
            return (reusefield[nr], nr)
        end
    end
    # if immortal or no reusable row was found, use the next row
    nr = nextidfield
    nextidfield = nextidfield + 1
    if nr > length(reusefield)
        resize!(reusefield, nr)
    end
    reusefield[nr] = 0
    (0, nr)
end

#################### Mortal, Stateless
# for Vectors (immortal agents) we set reuse always fix to 0
nff_mortal_stateless = NodeFieldFactory(
    type = (T, _) -> :(Vector{Bool}),
    constructor = (T, _) -> :(Vector{Bool}()),
    init_field = (T, info, simsymbol) -> begin
        if haskey(info.nodes_attr[T], :size)
            s = info.nodes_attr[T][:size]
            @eval function init_field!(sim::$simsymbol, ::Type{$T})
                sim.$(readfield(T)) = Vector{Bool}()
                sim.$(writefield(T)) = Vector{Bool}()
                resize!(sim.$(writefield(T)), $s)
                # create the additional structures we use to
                # track the state of the rows 
                attrs = sim.typeinfos.nodes_attr[$T]
                attrs[:reuseable] = Vector{AgentNr}()
                attrs[:died] = BitVector()
                resize!(attrs[:died], $s)
            end
        else
            @eval function init_field!(sim::$simsymbol, ::Type{$T})
                sim.$(readfield(T)) = Vector{Bool}()
                sim.$(writefield(T)) = Vector{Bool}()
                # create the additional structures we use to
                # track the state of the rows 
                attrs = sim.typeinfos.nodes_attr[$T]
                attrs[:reuseable] = Vector{AgentNr}()
                attrs[:died] = BitVector()
            end
        end
    end,
    
    add_agent = (T, info, simsymbol) -> begin
        if haskey(info.nodes_attr[T], :size)
            # TODO: remove the @eval for the typeids?
            @eval typeid = $info.nodes_type2id[$T]
            # As the agent is stateless, we store only true or false for living/died.
            # we track this already in :died, but MPI RMA does not work for
            # bitvectors
            @eval function add_agent!(sim::$simsymbol, agent::$T)
                (reuse, nr) = _get_next_id(sim, $T, sim.$(reusefield(T)),
                                           sim.$(nextidfield(T)), true)
                @inbounds sim.$(writefield(T))[nr] = true
                agent_id($typeid, reuse, nr) 
            end
        else
            @eval typeid = $info.nodes_type2id[$T]
            @eval function add_agent!(sim::$simsymbol, agent::$T)
                (reuse, nr) = _get_next_id(sim, $T, sim.$(reusefield(T)),
                                           sim.$(nextidfield(T)), true)
                if nr > length(sim.$(writefield(T)))
                    resize!(sim.$(writefield(T)), nr)
                end
                @inbounds sim.$(writefield(T))[nr] = true
                agent_id($typeid, reuse, nr)
            end
        end
    end,

    agentstate = (T, _, simsymbol) -> begin
        @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
            @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[$T] AGENTSTATE_MSG
            
            r = process_nr(id)
            if r == mpi.rank
                @inbounds sim.$(readfield(T))[agent_nr(id)]
            else
                win = sim.typeinfos.nodes_attr[$T][:window]
                as = Vector{$T}(undef, 1)
                MPI.Win_lock(win; rank = r, type = :shared, nocheck = true)
                MPI.Get!(as, r, agent_nr(id) - 1, win)
                MPI.Win_unlock(win; rank = r)
                @inbounds as[1]
            end
        end
    end,

    transition = (T, info, simsymbol) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function transition!(sim::$simsymbol, func, ::Type{$T})
            read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
            write::Vector{$T} = sim.nodes_id2write[$typeid](sim)
            attrs = sim.typeinfos.nodes_attr[$T]
            # an own counter (with the correct type) is faster then enumerate 
            idx = AgentNr(0)
            for state::$T in read
                idx += AgentNr(1)
                # jump over died agents
                if ! attrs[:died][idx]
                    newstate = func(state, agent_id(sim, $typeid, idx), sim)
                    if isnothing(newstate)
                        attrs[:died][idx] = true
                        push!(attrs[:reuseable], idx)
                    else
                        write[idx] = newstate
                    end
                end
            end 
        end
        @eval function transition_invariant_compute!(sim::$simsymbol, func, ::Type{$T})
            read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
            attrs = sim.typeinfos.nodes_attr[$T]
            # an own counter (with the correct type) is faster then enumerate 
            idx = AgentNr(0)
            for state::$T in read
                idx += AgentNr(1)
                # jump over died agents
                if ! attrs[:died][idx]
                    func(state, immortal_agent_id($typeid, idx), sim)
                end
            end 
        end
    end,

    prepare_write = (T, _, simsymbol) -> begin
        # add_existing can be ignored, as immortal agents can not be in the
        # rebuild vector anyway
        @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
            sim.$(writefield(T)) = deepcopy(sim.$(readfield(T)))
        end
    end,

    finish_write = (T, _, simsymbol) -> begin
        @eval function finish_write!(sim::$simsymbol, ::Type{$T})
            # finish the last epoch
            if haskey(sim.typeinfos.nodes_attr[$T], :window)
                win = sim.typeinfos.nodes_attr[$T][:window]
                MPI.free(win)
                delete!(sim.typeinfos.nodes_attr[$T], :window)
            end
            sim.$(readfield(T)) = sim.$(writefield(T))
            # TODO: Add infokeys
            win = MPI.Win_create(sim.$(readfield(T)), MPI.COMM_WORLD)
            sim.typeinfos.nodes_attr[$T][:window] = win
        end
    end,


    aggregate = (T, _, simsymbol) -> begin
        @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, sim.$(readfield(T)); kwargs...)
        end
    end, 
)



#################### Vec, Distributed, Immortal
# for Vectors (immortal agents) we set reuse always fix to 0
nff_immortal = NodeFieldFactory(
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
            r = process_nr(id)
            if r == mpi.rank
                @inbounds sim.$(readfield(T))[agent_nr(id)]
            else
                win = sim.typeinfos.nodes_attr[$T][:window]
                as = Vector{$T}(undef, 1)
                MPI.Win_lock(win; rank = r, type = :shared, nocheck = true)
                MPI.Get!(as, r, agent_nr(id) - 1, win)
                MPI.Win_unlock(win; rank = r)
                @inbounds as[1]
            end
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
        @eval function transition_invariant_compute!(sim::$simsymbol, func, ::Type{$T})
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
        # add_existing can be ignored, as immortal agents can not be in the
        # rebuild vector anyway
        @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
            sim.$(writefield(T)) = deepcopy(sim.$(readfield(T)))
        end
    end,

    finish_write = (T, _, simsymbol) -> begin
        @eval function finish_write!(sim::$simsymbol, ::Type{$T})
            # finish the last epoch
            if haskey(sim.typeinfos.nodes_attr[$T], :window)
                win = sim.typeinfos.nodes_attr[$T][:window]
                MPI.free(win)
                delete!(sim.typeinfos.nodes_attr[$T], :window)
            end
            sim.$(readfield(T)) = sim.$(writefield(T))
            # TODO: Add infokeys
            win = MPI.Win_create(sim.$(readfield(T)), MPI.COMM_WORLD)
            sim.typeinfos.nodes_attr[$T][:window] = win
        end
    end,


    aggregate = (T, _, simsymbol) -> begin
        @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, sim.$(readfield(T)); kwargs...)
        end
    end, 
)


#################### Vec, Distributed, Mortal
# For mortal agents, we are still using a vector, but check for gaps
# due to died agents.
# We also track them via a bitarray, and a set of free indicies
nff_mortal = NodeFieldFactory(
    init_field = (T, info, simsymbol) -> begin
        if haskey(info.nodes_attr[T], :size)
            s = info.nodes_attr[T][:size]
            @eval function init_field!(sim::$simsymbol, ::Type{$T})
                sim.$(readfield(T)) = Vector{$T}()
                sim.$(writefield(T)) = Vector{$T}()
                resize!(sim.$(writefield(T)), $s)
                # create the additional structures we use to
                # track the state of the rows 
                attrs = sim.typeinfos.nodes_attr[$T]
                attrs[:reuseable] = Vector{AgentNr}()
                attrs[:died] = BitVector()
                resize!(attrs[:died], $s)
            end
        else
            @eval function init_field!(sim::$simsymbol, ::Type{$T})
                sim.$(readfield(T)) = Vector{$T}()
                sim.$(writefield(T)) = Vector{$T}()
                # create the additional structures we use to
                # track the state of the rows 
                attrs = sim.typeinfos.nodes_attr[$T]
                attrs[:reuseable] = Vector{AgentNr}()
                attrs[:died] = BitVector()
            end
        end
    end,
    
    add_agent = (T, info, simsymbol) -> begin
        if haskey(info.nodes_attr[T], :size)
            @eval typeid = $info.nodes_type2id[$T]
            @eval function add_agent!(sim::$simsymbol, agent::$T)
                (reuse, nr) = _get_next_id(sim, $T, sim.$(reusefield(T)),
                                           sim.$(nextidfield(T)), true)
                @inbounds sim.$(writefield(T))[nr] = agent
                agent_id($typeid, Reuse(reuse), nr)
            end
        else
            @eval typeid = $info.nodes_type2id[$T]
            @eval function add_agent!(sim::$simsymbol, agent::$T)
                (reuse, nr) = _get_next_id(sim, $T, sim.$(reusefield(T)),
                                           sim.$(nextidfield(T)), true)
                if nr > length(sim.$(writefield(T)))
                    resize!(sim.$(writefield(T)), nr)
                end
                @inbounds sim.$(writefield(T))[nr] = agent
                agent_id($typeid, Reuse(reuse), nr)
            end
        end
    end,

    agentstate = (T, _, simsymbol) -> begin
        @eval function agentstate(sim::$simsymbol, id::AgentID, ::Type{$T})
            @mayassert type_nr(id) == sim.typeinfos.nodes_type2id[$T] AGENTSTATE_MSG
            r = process_nr(id)
            if r == mpi.rank
                @inbounds sim.$(readfield(T))[agent_nr(id)]
            else
                win = sim.typeinfos.nodes_attr[$T][:window]
                as = Vector{$T}(undef, 1)
                MPI.Win_lock(win; rank = r, type = :shared, nocheck = true)
                MPI.Get!(as, r, agent_nr(id) - 1, win)
                MPI.Win_unlock(win; rank = r)
                @inbounds as[1]
            end
        end
    end,

    transition = (T, info, simsymbol) -> begin
        @eval typeid = $info.nodes_type2id[$T]
        @eval function transition!(sim::$simsymbol, func, ::Type{$T})
            read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
            write::Vector{$T} = sim.nodes_id2write[$typeid](sim)
            attrs = sim.typeinfos.nodes_attr[$T]
            # an own counter (with the correct type) is faster then enumerate 
            idx = AgentNr(0)
            for state::$T in read
                idx += AgentNr(1)
                # jump over died agents
                if ! attrs[:died][idx]
                    newstate = func(state, agent_id(sim, $typeid, idx), sim)
                    if isnothing(newstate)
                        attrs[:died][idx] = true
                        push!(attrs[:reuseable], idx)
                    else
                        write[idx] = newstate
                    end
                end
            end 
        end
        @eval function transition_invariant_compute!(sim::$simsymbol, func, ::Type{$T})
            read::Vector{$T} = sim.nodes_id2read[$typeid](sim)
            attrs = sim.typeinfos.nodes_attr[$T]
            # an own counter (with the correct type) is faster then enumerate 
            idx = AgentNr(0)
            for state::$T in read
                idx += AgentNr(1)
                # jump over died agents
                if ! attrs[:died][idx]
                    func(state, immortal_agent_id($typeid, idx), sim)
                end
            end 
        end
    end,

    prepare_write = (T, _, simsymbol) -> begin
        # add_existing can be ignored, as immortal agents can not be in the
        # rebuild vector anyway
        @eval function prepare_write!(sim::$simsymbol, add_existing::Bool, ::Type{$T})
            sim.$(writefield(T)) = deepcopy(sim.$(readfield(T)))
        end
    end,

    finish_write = (T, _, simsymbol) -> begin
        @eval function finish_write!(sim::$simsymbol, ::Type{$T})
            # finish the last epoch
            if haskey(sim.typeinfos.nodes_attr[$T], :window)
                win = sim.typeinfos.nodes_attr[$T][:window]
                MPI.free(win)
                delete!(sim.typeinfos.nodes_attr[$T], :window)
            end
            sim.$(readfield(T)) = sim.$(writefield(T))
            # TODO: Add infokeys
            win = MPI.Win_create(sim.$(readfield(T)), MPI.COMM_WORLD)
            sim.typeinfos.nodes_attr[$T][:window] = win
        end
    end,


    aggregate = (T, _, simsymbol) -> begin
        @eval function aggregate(sim::$simsymbol, f, op, ::Type{$T}; kwargs...)
            mapreduce(f, op, sim.$(readfield(T)); kwargs...)
        end
    end, 
)


nffs = Dict(:Dict => nff_mortal,
            :Vector => nff_immortal,
            :Stateless => nff_mortal_stateless)

