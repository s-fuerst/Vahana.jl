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

struct StatelessAgent end

struct EdgeState
    f::AgentID
    t::AgentID
end

struct StatelessEdge end

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

function createsim(model, distribute = true)
    sim = model |>
        new_simulation(Params(1,2), Globals(1,2); logging = true, debug = true)

    ids = add_agents!(sim, [ Agent(InnerStruct(i,i+1),i+2) for i in 1:(5 * mpi.size) ])

    for id in ids
        add_edge!(sim, id, id, EdgeState(id, id))
        add_edge!(sim, id, ids[2], StatelessEdge())
        add_edge!(sim, id, ids[2], EdgeState(id, ids[1]))
    end

    add_agent!(sim, StatelessAgent())

    add_raster!(sim, :fourdspace, (2,2,2,1), _ -> StatelessAgent())

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

    apply_transition!(sim, [ Agent ], [ Agent, EdgeState ], [ Agent ]) do state, id, sim
        r = num_neighbors(sim, id, EdgeState) == 1 ? nothing : state
        r
    end

    if write
        write_agents(sim, [ Agent ])
    end

    sim.params = Params(3,4)
    
    if write
        write_params(sim)
        close(sim.h5file)
    end

    sim
end

function restore(sim)
    restored = new_simulation(model, Params(3,4), Globals(3,4))

    read_agents!(restored, sim.name)
    restored
end

function test_write_restore(model)
    sim = runsim(model, true)
    restored = restore(sim)
    test(sim, restored)
    
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

