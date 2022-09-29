using Test

using Vahana

import Vahana.has_trait

using MPI

using Logging

enable_asserts(true)

suppress_warnings(true)

# Logging.disable_logging(Logging.Info)

#MPI.set_errorhandler!(MPI.COMM_WORLD, MPI.ERRORS_RETURN)

# All ids are the initial ids 

@assert mod(mpi.size, 2) == 0 """
We need as minimum 2 PEs and also an even number of PEs"""

struct AgentState1
    idx::Int64
end

struct AgentState2
    idx::Int64
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
struct MPIEdgeEI fromid::AgentID; toid::AgentID end
struct MPIEdgeTI fromid::AgentID; toid::AgentID end
struct MPIEdgeSEI end
struct MPIEdgeSTI end
struct MPIEdgeSETI end

struct MPIEdgeTs fromid::AgentID; toid::AgentID end
struct MPIEdgeTsI fromid::AgentID; toid::AgentID end

struct MPIEdgeSTs end
struct MPIEdgeSTsI end
struct MPIEdgeSETsI end

statelessMPIEdgeTypes = [ MPIEdgeS, MPIEdgeSE, MPIEdgeST, MPIEdgeSI, MPIEdgeSEI, MPIEdgeSTI, MPIEdgeSETI, MPIEdgeSTs, MPIEdgeSTsI, MPIEdgeSETsI  ]

statefulMPIEdgeTypes = [ MPIEdgeD, MPIEdgeE, MPIEdgeT, MPIEdgeI, MPIEdgeEI, MPIEdgeTI, MPIEdgeTs, MPIEdgeTsI ]

model = ModelTypes() |>
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
    register_edgetype!(MPIEdgeEI, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeTI, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeSEI, :Stateless, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeSTI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeSETI, :Stateless, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1) |>
    register_edgetype!(MPIEdgeTs, :SingleAgentType; to_agenttype = AgentState1, size = mpi.size * 2) |>
    register_edgetype!(MPIEdgeTsI, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1, size = mpi.size* 2) |>
    register_edgetype!(MPIEdgeSTs, :Stateless, :SingleAgentType; to_agenttype = AgentState1, size = mpi.size * 2) |>
    register_edgetype!(MPIEdgeSTsI, :Stateless, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1, size = mpi.size * 2) |>
    register_edgetype!(MPIEdgeSETsI, :Stateless, :SingleEdge, :SingleAgentType, :IgnoreFrom; to_agenttype = AgentState1, size = mpi.size * 2) |>
    construct_model("MPI EdgeTypes");

# TODO: Implement agentstate, then we can check that the edges are correct
function check(ET)
    (agent, id, sim) -> begin
        @test has_neighbor(sim, id, ET)
    end
end

# we change the direction of the cycle, after the first call
# the edges are going from id to id-1 (mod mpi.size)
# as we use a hack that allows us to test also :IgnoreFrom edges,
# a second reverse does not result in the original state 
function reverse_edge_direction(ET)
    (state, id, sim) -> begin
        # In the :SingleEdge case there are no edges to AgentState2
        if has_trait(sim, ET, :SingleAgentType) && typeof(state) == AgentState2
            return
        end
        # we are know how the edges are constructed and how ids are given
        # so we use this information to create the needed ids even
        # for the :IgnoreFrom case
        target_rank = mod(state.idx - 2, mpi.size)
        target_id = AgentID(1 << Vahana.SHIFT_TYPE +
            target_rank << Vahana.SHIFT_RANK + 1)

        if has_trait(sim, ET, :Stateless)
            add_edge!(sim, id, target_id, ET())
        else
            if has_trait(sim, ET, :SingleEdge)
                nstate = edgestates(sim, id, ET) 
            else 
                nstate = edgestates(sim, id, ET) |> first
            end
            add_edge!(sim, id, target_id, nstate)
        end
    end
end

function testforedgetype(ET)
    sim = new_simulation(model, nothing, nothing)

    part = Dict{AgentID, UInt32}()

    if mpi.isroot
        agentids = add_agents!(sim, [ AgentState1(i) for i in 1:mpi.size ])
        agentids2 = add_agents!(sim, [ AgentState2(i, true) for i in 1:mpi.size ])

        for i in 1:mpi.size
            # the edges construct a directed cycle, going from id-1 to id
            fromid = agentids[mod1(i-1, mpi.size)]
            toid = agentids[i]
            toid2 = agentids2[i]
            if ! has_trait(sim, ET, :Stateless)
                add_edge!(sim, fromid, toid, ET(fromid, toid))
                if ! has_trait(sim, ET, :SingleAgentType)
                    add_edge!(sim, fromid, toid2, ET(fromid, toid))
                end
            else
                add_edge!(sim, fromid, toid, ET())
                if ! has_trait(sim, ET, :SingleAgentType)
                    add_edge!(sim, fromid, toid2, ET())
                end
            end
        end

        for i in 1:mpi.size
            part[agentids[i]] = i
            part[agentids2[i]] = mod1(i, 2)
        end
    end

    # copy the simulation, so that we can test with an manual partition
    # so that we can test that the agents/edges are moved to the expected
    # nodes and also the partitioning via Metis later
    simautopar = deepcopy(sim)

    @test num_agents(sim, AgentState1; write = true) == (mpi.isroot ? mpi.size : 0)
    @test num_agents(sim, AgentState2; write = true) == (mpi.isroot ? mpi.size : 0)

    num_edges_per_PE = has_trait(sim, ET, :SingleAgentType) ? 1 : 2
    @test num_edges(sim, ET; write = true) == (mpi.isroot ? mpi.size * num_edges_per_PE : 0)

    newidsmap = finish_init!(sim; partition = part, return_idmapping = true)
    
    apply_transition!(sim, check(ET), [ AgentState1 ], [], []; invariant_compute = true)
    
    @test num_agents(sim, AgentState1) == 1
    @test num_agents(sim, AgentState2) == (mpi.rank < 2 ? mpi.size / 2 : 0)

    
    if has_trait(sim, ET, :SingleAgentType)
        @test num_edges(sim, ET) == num_agents(sim, AgentState1) 
    else
        @test num_edges(sim, ET) ==
            num_agents(sim, AgentState1) + num_agents(sim, AgentState2)
    end

    apply_transition!(sim, check(ET), [ AgentState1 ], [], []; invariant_compute = true)

    # we are testing now that new edges in the transition functions are
    # send to the correct ranks
    apply_transition!(sim, reverse_edge_direction(ET),
                      [ AgentState1, AgentState2 ], [], [ ET ]; invariant_compute = true)

    # the edges are not send to other ranks before they are 
    apply_transition!(sim, check(ET), [ AgentState1 ], [ ET ], []; invariant_compute = true)
    
    if has_trait(sim, ET, :SingleAgentType)
        @test num_edges(sim, ET) == num_agents(sim, AgentState1) 
    else
        @test num_edges(sim, ET) ==
            num_agents(sim, AgentState1) + num_agents(sim, AgentState2)
    end
    
    # we check now the partitioning via Metis
    finish_init!(simautopar)

    if has_trait(sim, ET, :SingleAgentType)
        @test num_edges(sim, ET) == num_agents(sim, AgentState1) 
    else
        @test num_edges(sim, ET) ==
            num_agents(sim, AgentState1) + num_agents(sim, AgentState2)
    end

    
    
end

@testset "EdgeTypes" begin
    CurrentEdgeType = Nothing

    if CurrentEdgeType === Nothing
        for ET in [ statelessMPIEdgeTypes; statefulMPIEdgeTypes ]
            testforedgetype(ET)
        end
    else
        testforedgetype(CurrentEdgeType)
    end
    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end


