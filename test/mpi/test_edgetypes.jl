using Test

using Vahana

using MPI

enable_asserts(true)

suppress_warnings(true)

# All ids are the initial ids 

@assert mod(mpi.size, 2) == 0 """
We need as minimum 2 PEs and also an even number of PEs"""

struct AgentState1
    id::Int64
end

struct AgentState2
    id::Int64
    something::Bool
end

struct MPIEdgeD fromid::AgentID; toid::AgentID end # D for default (no trait is set)
struct MPIEdgeS end
struct MPIEdgeE fromid::AgentID; toid::AgentID end
struct MPIEdgeT fromid::AgentID; toid::AgentID end
struct MPIEdgeI fromid::AgentID; toid::AgentID end
struct MPIEdgeSE end
struct MPIEdgeST end
struct MPIEdgeSI end
struct MPIEdgeET fromid::AgentID; toid::AgentID end
struct MPIEdgeEI fromid::AgentID; toid::AgentID end
struct MPIEdgeTI fromid::AgentID; toid::AgentID end
struct MPIEdgeSET end
struct MPIEdgeSEI end
struct MPIEdgeSTI end
struct MPIEdgeETI fromid::AgentID; toid::AgentID end
struct MPIEdgeSETI end

struct MPIEdgeTs fromid::AgentID; toid::AgentID end
struct MPIEdgeTsI fromid::AgentID; toid::AgentID end

struct MPIEdgeSTs end
struct MPIEdgeSTsI end

statelessMPIEdgeTypes = [ MPIEdgeS, MPIEdgeSE, MPIEdgeST, MPIEdgeSI, MPIEdgeSET, MPIEdgeSEI, MPIEdgeSTI, MPIEdgeSETI, MPIEdgeSTs, MPIEdgeSTsI  ]

statefulMPIEdgeTypes = [ MPIEdgeD, MPIEdgeE, MPIEdgeT, MPIEdgeI, MPIEdgeET, MPIEdgeEI, MPIEdgeTI, MPIEdgeETI, MPIEdgeTs, MPIEdgeTsI ]

@time model = ModelTypes() |>
    register_agenttype!(AgentState1) |>
    register_agenttype!(AgentState2) |>
    register_edgetype!(MPIEdgeD) |>
    register_edgetype!(MPIEdgeS, :Stateless) |>
    register_edgetype!(MPIEdgeE, :SingleEdge) |>
    register_edgetype!(MPIEdgeT, :SingleAgentType; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeI, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeSE, :Stateless, :SingleEdge) |>
    register_edgetype!(MPIEdgeST, :Stateless, :SingleAgentType; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeSI, :Stateless, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeET, :SingleEdge, :SingleAgentType; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeEI, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeTI, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeSET, :Stateless, :SingleEdge, :SingleAgentType; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeSEI, :Stateless, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeSTI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeETI, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeSETI, :Stateless, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeTs, :SingleAgentType; to_agenttype = AgentState1, size = mpi.size) |>
    register_edgetype!(MPIEdgeTsI, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1, size = mpi.size) |>
    register_edgetype!(MPIEdgeSTs, :Stateless, :SingleAgentType; to_agenttype = AgentState1, size = mpi.size) |>
    register_edgetype!(MPIEdgeSTsI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1, size = mpi.size) |>
    construct_model("MPI EdgeTypes");


function testforedgetype(ET)
    sim = new_simulation(model, nothing, nothing)

    part = Dict{AgentID, ProcessID}()

    if mpi.isroot
        @info "===== run test for edgetype" ET
        agentids = add_agents!(sim, [ AgentState1(i) for i in 1:mpi.size ])
        agentids2 = add_agents!(sim, [ AgentState2(i, true) for i in 1:mpi.size ])
        

        for i in 1:mpi.size
            fromid = agentids[mod1(i-1, mpi.size)]
            toid = agentids[i]
            toid2 = agentids2[i]
            if fieldcount(ET) > 0
                add_edge!(sim, fromid, toid, ET(fromid, toid))
                if ! Vahana.has_trait(sim, ET, :SingleAgentType)
                    add_edge!(sim, fromid, toid2, ET(fromid, toid))
                end
            else
                add_edge!(sim, fromid, toid, ET())
                if ! Vahana.has_trait(sim, ET, :SingleAgentType)
                    add_edge!(sim, fromid, toid2, ET())
                end
            end
        end

        for i in 1:mpi.size
            part[agentids[i]] = i
            part[agentids2[i]] = mod1(i, 2)
        end
    end

    finish_init!(sim; partition = part)

    @test num_agents(sim, AgentState1) == (mpi.isroot ? mpi.size : 0)
    @test num_agents(sim, AgentState2) == (mpi.isroot ? mpi.size : 0)

    num_edges_per_PE = Vahana.has_trait(sim, ET, :SingleAgentType) ? 1 : 2
    @test num_edges(sim, ET) == (mpi.isroot ? mpi.size * num_edges_per_PE : 0)

    @info "Before distribute!" mpi.rank mpi.size 

    sleep(0.5)
    MPI.Barrier(mpi.comm)
    
    Vahana.distribute!(sim, part)

    @info "After distribute!" mpi.rank 

    @test num_agents(sim, AgentState1) == 1
    @test num_agents(sim, AgentState2) == (mpi.rank < 2 ? mpi.size / 2 : 0)

    if Vahana.has_trait(sim, ET, :SingleAgentType)
        @test num_edges(sim, ET) == num_agents(sim, AgentState1) 
    else
        @test num_edges(sim, ET) ==
            num_agents(sim, AgentState1) + num_agents(sim, AgentState2)
    end
end

CurrentEdgeType = MPIEdgeSEI


if CurrentEdgeType === Nothing
    for ET in [ statelessMPIEdgeTypes; statefulMPIEdgeTypes ]
        testforedgetype(ET)
    end
else
#    testforedgetype(MPIEdgeS)
#    testforedgetype(MPIEdgeSE)
#    testforedgetype(MPIEdgeST)
#    testforedgetype(MPIEdgeSI)
#   testforedgetype(CurrentEdgeType)
    testforedgetype(MPIEdgeSET)
    testforedgetype(MPIEdgeS)
#    testforedgetype(CurrentEdgeType)
#   testforedgetype(CurrentEdgeType)
end
