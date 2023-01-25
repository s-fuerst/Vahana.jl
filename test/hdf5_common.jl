using OhMyREPL

using Revise

using Vahana

using Test

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
    gb::Int64
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
        new_simulation(Params(1,2), Globals(1,2); logging = true, debug = true)

    ids = add_agents!(sim, [ Agent(InnerStruct(i,i+1),i+2) for i in 1:(16 * mpi.size) ])

    for id in ids
        add_edge!(sim, id, id, EdgeState(id, id))
        add_edge!(sim, id, ids[2], StatelessEdge())
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
    @info "start model $(model.name)" mpi.rank
    
    sim = createsim(model)

    if write
        write_snapshot(sim)
    end

    apply_transition!(sim, [ Agent ], [ Agent ], [ Agent ]) do state, id, sim
        Agent(state.inner, state.f - 2)
    end

    if write
        write_snapshot(sim)
    end


    sim.params = Params(3,4)
    
    if write
        write_params(sim)
        close(sim.h5file)
    end

    sim
end

function remove_some_rasternodes(state, id, sim)
    nstate = neighborstates(sim, id, RasterEdge, Agent) |> first
    mod1(nstate.f, 8) > 4.5 ? nothing : state
end    
    

function restore(sim)
    restored = new_simulation(model, Params(3,4), Globals(3,4))
    read_snapshot!(restored, sim.name)
    restored
end

function test_write_restore(model)
    sim = runsim(model, true)
    restored = restore(sim)
    test(sim, restored)

    apply_transition!(sim,
                      remove_some_rasternodes,
                      [ RasterAgent ],
                      [ RasterAgent, RasterEdge, Agent ],
                      [ RasterAgent ])

    apply_transition!(restored,
                      remove_some_rasternodes,
                      [ RasterAgent ],
                      [ RasterAgent, RasterEdge, Agent ],
                      [ RasterAgent ])

    
    finish_simulation!(sim)
    finish_simulation!(restored)
end


function test(sim, restored)
    @test sim.Agent.read.died == restored.Agent.read.died
    @test sim.Agent.read.state == restored.Agent.read.state
    @test sim.Agent.read.reuseable == restored.Agent.read.reuseable
    @test sim.Agent.nextid == restored.Agent.nextid

    apply_transition!(sim, sumi2o, [ Agent ], [ Agent ], [ Agent ])

    restored.initialized = true

    apply_transition!(restored, sumi2o, [ Agent ], [ Agent ], [ Agent ])

    @test sim.Agent.read.state == restored.Agent.read.state
end

