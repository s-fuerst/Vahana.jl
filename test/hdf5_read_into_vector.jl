using Vahana, Test

# set_hdf5_path(joinpath(dirname(@__FILE__), "h5"))
set_hdf5_path(tempdir())

struct Agent
    rank::Int32
end

struct RankEdge
    rank::Int32
end

model = ModelTypes() |>
    register_agenttype!(Agent) |>
    register_edgetype!(RankEdge) |>
    create_model("Test readagents_readedges")

function create_sim_and_testfile(nrAgents)
    sim = create_simulation(model)

    for _ in 1:nrAgents
        add_agent!(sim, Agent(-1))
    end

    finish_init!(sim,  partition_algo = :EqualAgentNumbers)

    # we can now set the correct rank information and
    # create the edges
    apply!(sim, Agent, Agent, [ Agent, RankEdge ]) do self, id, sim
        add_edge!(sim, id, id, RankEdge(mpi.rank))
        Agent(mpi.rank)
    end

    write_snapshot(sim)

    filename = sim.filename

    finish_simulation!(sim)
    
    filename
end

@testset "read_agents" begin
    filename = create_sim_and_testfile(mpi.size)
    agents = read_agents(filename, Agent)
    @test length(agents) == 1
    @test agents[1].rank == mpi.rank

    filename = create_sim_and_testfile(0)
    agents = read_agents(filename, Agent)
    @test length(agents) == 0

    if mpi.size > 1
        filename = create_sim_and_testfile(1)
        agents = read_agents(filename, Agent)
        if mpi.rank == 0
            @test length(agents) == 1
            @test agents[1].rank == mpi.rank
        else
            @test agents == Vector()
        end
    end

    sleep(mpi.rank * 0.05)
end

@testset "read_edges" begin
    filename = create_sim_and_testfile(mpi.size)
    edges_ = read_edges(filename, RankEdge)
    @test length(edges_) == 1
    @test edges_[1].edge.state.rank == mpi.rank
   
    filename = create_sim_and_testfile(0)
    edges_ = read_edges(filename, RankEdge)
    @test length(edges_) == 0

    if mpi.size > 1
        filename = create_sim_and_testfile(1)
        edges_ = read_edges(filename, RankEdge)
        if mpi.rank == 0
            @test length(edges_) == 1
            @test edges_[1].edge.state.rank == mpi.rank
        else
            @test edges_ == Vector()
        end
    end

    sleep(mpi.rank * 0.05)
end
