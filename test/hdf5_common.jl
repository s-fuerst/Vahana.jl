using Vahana

using Test

suppress_warnings(true)

struct InnerStruct
    f::Float64
    i::Int64
end

struct Agent
    inner::InnerStruct
    f::Float64
end

struct RasterAgent end

struct EdgeState
    f::AgentID
    t::AgentID
end

struct StatelessEdge end

struct RasterEdge
    a::Int64
end

struct Globals
    ga::Int64
    gb::Vector{Float64}
    gc::Vector{Int64}
end

struct Params
    pa::Int64
    pb::Int64
end

function sumi2o(state, _, _)
    Agent(state.inner, state.inner.f + state.inner.i)
end

# the test model has the following structure:
# we create mpi.size * 8 agent of type Agent

function createsim(model, distribute = true)
    sim = model |>
        new_simulation(Params(1,2), Globals(1,[2.0],[3]); logging = true, debug = true)

    ids = add_agents!(sim, [ Agent(InnerStruct(i,i+1),i+2) for i in 1:16 ])

    for id in ids
        add_edge!(sim, id, id, EdgeState(id, id))
        add_edge!(sim, id, ids[2], StatelessEdge())
        add_edge!(sim, id, ids[5], StatelessEdge())
        add_edge!(sim, id, ids[2], EdgeState(id, ids[1]))
    end

    rids = add_raster!(sim, :fourdspace, (2,2,2,1), _ -> RasterAgent())

    for id in ids
        add_edge!(sim, id, rids[mod1(id, 8)], RasterEdge(id % 8 + 1))
    end
    

    finish_init!(sim;
                 partition_algo = :EqualAgentNumbers, distribute = distribute)

    sim
end    

function runsim(model, write)
    sim = createsim(model)

    if write
        write_snapshot(sim, "Initial state")
    end

    apply_transition!(sim, [ Agent ], [ Agent ], [ Agent ]) do state, id, sim
        Agent(state.inner, state.f - 2)
    end

    push_global!(sim, :gb, 2.1)
    push_global!(sim, :gc, 4)

    if write
        write_snapshot(sim)
    end

    apply_transition!(sim, remove_some_rasternodes, 
                      [ RasterAgent ],
                      [ RasterAgent, RasterEdge, Agent ],
                      [ RasterAgent, RasterEdge ])
    
    if write
        write_snapshot(sim, "Final state")
        close(sim.h5file)
    end

    sim
end

function remove_some_rasternodes(state, id, sim)
    nstate = neighborstates(sim, id, RasterEdge, Agent) |> first
    e = edges_to(sim, id, RasterEdge) |> first
    add_edge!(sim, e.from, id, RasterEdge(e.state.a * 2))
    mod1(nstate.f, 8) > 4.5 ? nothing : state
end    

function restore(model, sim; kwargs...)
    restored = new_simulation(model, Params(0,0), Globals(0,[0], [0]))
    read_snapshot!(restored, sim.name; kwargs...)
end

