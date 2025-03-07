
using Test

using Vahana

import Vahana.has_hint

using MPI

using Logging

import Graphs: SimpleGraphs

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

struct MPIEdgeD fromid::AgentID; toid::AgentID end # D for default (no hint is set)
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
struct SendId end

statelessMPIEdgeTypes = [ MPIEdgeS, MPIEdgeSE, MPIEdgeST, MPIEdgeSI, MPIEdgeSEI, MPIEdgeSTI, MPIEdgeSETI, MPIEdgeSTs, MPIEdgeSTsI, MPIEdgeSETsI  ]

statefulMPIEdgeTypes = [ MPIEdgeD, MPIEdgeE, MPIEdgeT, MPIEdgeI, MPIEdgeEI, MPIEdgeTI, MPIEdgeTs, MPIEdgeTsI ]

model = ModelTypes() |>
    register_agenttype!(AgentState1) |>
    register_agenttype!(AgentState2) |>
    register_edgetype!(MPIEdgeD) |>
    register_edgetype!(MPIEdgeS, :Stateless) |>
    register_edgetype!(MPIEdgeE, :SingleEdge) |>
    register_edgetype!(MPIEdgeT, :SingleType; target = AgentState1) |>
    register_edgetype!(MPIEdgeI, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeSE, :Stateless, :SingleEdge) |>
    register_edgetype!(MPIEdgeST, :Stateless, :SingleType; target = AgentState1) |>
    register_edgetype!(MPIEdgeSI, :Stateless, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeEI, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeTI, :SingleType, :IgnoreFrom; target = AgentState1) |>
    register_edgetype!(MPIEdgeSEI, :Stateless, :SingleEdge, :IgnoreFrom) |>
    register_edgetype!(MPIEdgeSTI, :Stateless, :SingleType, :IgnoreFrom; target = AgentState1) |>
    register_edgetype!(MPIEdgeSETI, :Stateless, :SingleEdge, :SingleType, :IgnoreFrom; target = AgentState1) |>
    register_edgetype!(MPIEdgeTs, :SingleType; target = AgentState1, size = mpi.size * 2) |>
    register_edgetype!(MPIEdgeTsI, :SingleType, :IgnoreFrom; target = AgentState1, size = mpi.size* 2) |>
    register_edgetype!(MPIEdgeSTs, :Stateless, :SingleType; target = AgentState1, size = mpi.size * 2) |>
    register_edgetype!(MPIEdgeSTsI, :Stateless, :SingleType, :IgnoreFrom; target = AgentState1, size = mpi.size * 2) |>
    register_edgetype!(MPIEdgeSETsI, :Stateless, :SingleEdge, :SingleType, :IgnoreFrom; target = AgentState1, size = mpi.size * 2) |>
    register_edgetype!(SendId, :SingleEdge) |>
    create_model("MPI EdgeTypes");


###
# We have the following edge setup (as an example for mpi.size of 3, but
# everything is modulo mpi.size)

# AS1_1 -> AS1_2
# AS1_1 -> AS2_2
# AS1_2 -> AS1_3
# AS1_2 -> AS2_3
# AS1_3 -> AS1_1
# AS1_3 -> AS2_1

function check(ET)
    (agent, id, sim) -> begin
        @test has_edge(sim, id, ET)
    end
end

# non reversed
function check_state(ET)
    (agent, id, sim) -> begin
        if has_hint(sim, ET, :SingleEdge)
            @test has_edge(sim, id, ET)
            if ! has_hint(sim, ET, :IgnoreFrom)
                @test neighborstates(sim, id, ET, AgentState1).idx ==
                    mod1(agent.idx - 1, mpi.size)
            end
        else
            @test num_edges(sim, id, ET) == 1
            if ! has_hint(sim, ET, :IgnoreFrom)
                @test first(neighborstates(sim, id, ET, AgentState1)).idx ==
                    mod1(agent.idx - 1, mpi.size)
            end
        end
    end
end

# reversed

# AS1_1 <- AS1_2
# AS1_1 <- AS2_2
# AS1_2 <- AS1_3
# AS1_2 <- AS2_3
# AS1_3 <- AS1_1
# AS1_3 <- AS2_1

# so the edges are going only to AgentState1. The edges from AS2 are
# dropped for the :SingleEdge and :SingleType hints.
function check_state_rev1(ET)
    (agent::AgentState1, id, sim) -> begin
        if has_hint(sim, ET, :SingleEdge) 
            @test has_edge(sim, id, ET)
            if ! has_hint(sim, ET, :IgnoreFrom)
                @test neighborstates(sim, id, ET, AgentState1).idx ==
                    mod1(agent.idx + 1, mpi.size)
            end
        elseif has_hint(sim, ET, :SingleType)
            @test num_edges(sim, id, ET) == 1
            if ! has_hint(sim, ET, :IgnoreFrom)
                @test first(neighborstates(sim, id, ET, AgentState1)).idx ==
                    mod1(agent.idx + 1, mpi.size)
            end
        else
            @test num_edges(sim, id, ET) == 2
            if ! has_hint(sim, ET, :IgnoreFrom)
                @test first(neighborstates_flexible(sim, id, ET)).idx ==
                    mod1(agent.idx + 1, mpi.size)
            end
        end
    end
end

# check that nothing is going to the AS2 agents anymore
function check_state_rev2(ET)
    (_, id, sim) -> begin
        # for the SingleType we determined that ET can only go to AS1 agents
        if ! has_hint(sim, ET, :SingleType)
            @test ! has_edge(sim, id, ET)
        end
    end
end


# we change the direction of the cycle, after the first call
# the edges are going from id to id-1 (mod mpi.size)
# as we use a hack that allows us to test also :IgnoreFrom edges,
# a second reverse does not result in the original state 
function reverse_edge_direction(ET)
    (state, id, sim) -> begin
        # In the :SingleType case there are no edges to AgentState2
        if has_hint(sim, ET, :SingleType) && typeof(state) == AgentState2
            return
        end
        # If we would reverse the edges in the :SingleEdge case,
        # two edges would point to the same agent.
        if has_hint(sim, ET, :SingleEdge) && typeof(state) == AgentState2
            return
        end
        # we are know how the edges are constructed and how ids are given
        # so we use this information to create the needed ids even
        # for the :IgnoreFrom case
        target_rank = mod(state.idx - 2, mpi.size)
        target_id = AgentID(1 << Vahana.SHIFT_TYPE +
            target_rank << Vahana.SHIFT_RANK + 1)

        if has_hint(sim, ET, :Stateless)
            add_edge!(sim, id, target_id, ET())
        else
            if has_hint(sim, ET, :SingleEdge)
                nstate = edgestates(sim, id, ET) 
            else 
                nstate = edgestates(sim, id, ET) |> first
            end
            add_edge!(sim, id, target_id, nstate)
        end
    end
end

function testforedgetype(ET)
    sim = create_simulation(model, nothing, nothing)

    part = Dict{AgentID, UInt32}()

    # construct the edges as described above
    if mpi.isroot
        agentids = add_agents!(sim, [ AgentState1(i) for i in 1:mpi.size ])
        agentids2 = add_agents!(sim, [ AgentState2(i, true) for i in 1:mpi.size ])

        for i in 1:mpi.size
            # the edges construct a directed cycle, going from id-1 to id
            fromid = agentids[mod1(i-1, mpi.size)]
            toid = agentids[i]
            toid2 = agentids2[i]
            if ! has_hint(sim, ET, :Stateless)
                add_edge!(sim, fromid, toid, ET(fromid, toid))
                if ! has_hint(sim, ET, :SingleType)
                    add_edge!(sim, fromid, toid2, ET(fromid, toid))
                end
            else
                add_edge!(sim, fromid, toid, ET())
                if ! has_hint(sim, ET, :SingleType)
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
    @test num_agents(sim, AgentState1, false) == (mpi.isroot ? mpi.size : 0)
    @test num_agents(sim, AgentState2, false) == (mpi.isroot ? mpi.size : 0)
    @test num_agents(sim, AgentState1) == mpi.size 
    @test num_agents(sim, AgentState2) == mpi.size 

    num_edges_per_PE = has_hint(sim, ET, :SingleType) ? 1 : 2
    @test num_edges(sim, ET; write = true) == mpi.size * num_edges_per_PE 
    
    finish_init!(sim; partition_algo = :EqualAgentNumbers)

    apply!(sim, check_state(ET), [ AgentState1 ],
           [ AgentState1, ET ], [])
    
    @test num_agents(sim, AgentState1, false) == 1
    @test num_agents(sim, AgentState2, false) == 1
    @test num_agents(sim, AgentState1) == mpi.size
    @test num_agents(sim, AgentState2) == mpi.size 

    @test num_edges(sim, ET; write = true) == mpi.size * num_edges_per_PE 
    # we are testing now that new edges in the transition functions are
    # send to the correct ranks
    apply!(sim, reverse_edge_direction(ET),
           [ AgentState1, AgentState2 ],
           [ AgentState1, AgentState2, ET ],
           [ ET ])


    apply!(sim, check_state_rev1(ET),
           [ AgentState1 ],
           [ AgentState1, AgentState2, ET ],
           [])

    apply!(sim, check_state_rev2(ET),
           [ AgentState2 ],
           [ ET ], [])

    finish_simulation!(sim)
end

# @testset "EdgeTypes" begin
#     CurrentEdgeType = Nothing

#     if CurrentEdgeType === Nothing
#         for ET in [ statelessMPIEdgeTypes; statefulMPIEdgeTypes ]
#             testforedgetype(ET)
#         end
#     else
#         testforedgetype(CurrentEdgeType)
#     end
#     # this hack should help that the output is not scrambled
#     sleep(mpi.rank * 0.05)
# end

###
function testforedgetype_remove(ET)
    sim = create_simulation(model, nothing, nothing)

    # each agent has exact one neighbar
    if has_hint(sim, ET, :Stateless)
        add_graph!(sim,
                   SimpleGraphs.cycle_digraph(100),
                   i -> AgentState1(i),
                   _ -> ET())
    else
        add_graph!(sim,
                   SimpleGraphs.cycle_digraph(100),
                   i -> AgentState1(i),
                   _ -> ET(0, 0))
    end

    finish_init!(sim)

    removed = apply(sim, AgentState1, [AgentState1, ET], ET;
                    add_existing = ET) do self, id, sim
                        if self.idx % 2 == 0
                            remove_edges!(sim, id, ET)
                        end
                    end
    @test num_edges(removed, ET) == 50
    finish_simulation!(removed)

    copy = copy_simulation(sim)

    if ! has_hint(sim, ET, :IgnoreFrom)
        removed = apply(sim, AgentState1, [AgentState1, ET], ET;
                        add_existing = ET) do self, id, sim
                            if self.idx % 2 == 0
                                nid = neighborids(sim, id, ET) |> first
                                remove_edges!(sim, nid, ET)
                            end
                        end
        @test num_edges(removed, ET) == 50
        finish_simulation!(removed)
    end
    finish_simulation!(sim)

    # remove_edges! and add_edge! for the same edge should keep the edge
    # (the order depends on the handling if the send edges)
    if ! has_hint(sim, ET, :IgnoreFrom)
        apply!(copy, AgentState1, ET, ET; add_existing = ET) do self, id, sim
            fromid = neighborids(sim, id, ET) |> first
            remove_edges!(sim, fromid, id, ET)
            if has_hint(sim, ET, :Stateless)
                add_edge!(sim, fromid, id, ET())
            else
                add_edge!(sim, fromid, id, ET(0,0))
            end
        end
        @test num_edges(copy, ET) == 100
        
    end
    
    finish_simulation!(copy)
    
end

function testforedgetype_remove_from(ET)
    sim = create_simulation(model, nothing, nothing)

    graph = if has_hint(sim, ET, :SingleEdge)
        SimpleGraphs.cycle_digraph(100)
    else
        SimpleGraphs.complete_graph(100)
    end

    if has_hint(sim, ET, :Stateless)
        add_graph!(sim,
                   graph,
                   i -> AgentState1(i),
                   _ -> ET())
    else
        add_graph!(sim,
                   graph,
                   i -> AgentState1(i),
                   _ -> ET(0, 0))
    end

    finish_init!(sim)
    if has_hint(sim, ET, :SingleEdge)
        @assert num_edges(sim, ET) == 100 # all 100 agents have 1 edge
    else
        @assert num_edges(sim, ET) == 9900 # all 100 agents have 99 edges
    end
    sim2 = copy_simulation(sim)
    sim3 = copy_simulation(sim)

    apply!(sim, AgentState1, [AgentState1, ET], ET;
           add_existing = ET) do self, id, sim
               nids = neighborids(sim, id, ET) 
               remove_edges!(sim, first(nids), id, ET)
               if ! has_hint(sim, ET, :SingleEdge)
                   remove_edges!(sim, nids[2], id, ET)
               end
           end
    if has_hint(sim, ET, :SingleEdge)
        @test num_edges(sim, ET) == 0
    else
        @test num_edges(sim, ET) == 9700 # we remove for 100 agent 2 edges
    end

    # test also to remove the edges in the other direction
    if !has_hint(sim, ET, :SingleEdge)
        apply!(sim2, AgentState1, [AgentState1, ET], ET;
               add_existing = ET) do self, id, sim
                   if self.idx % 2 == 0
                       nid = rand(neighborids(sim, id, ET))
                       remove_edges!(sim, id, nid, ET)
                   end
               end
        @test num_edges(sim2, ET) == 9850 # we remove for 50 agent 2 edges
    else
        apply!(sim2, AgentState1, ET, SendId) do _, id, sim
            nid = neighborids(sim, id, ET)
            add_edge!(sim2, id, nid, SendId())
        end
        apply!(sim2, AgentState1, [AgentState1, ET, SendId], ET;
               add_existing = ET) do self, id, sim
                   if self.idx < 100
                       nid = neighborids(sim, id, SendId)
                       remove_edges!(sim, id, nid, ET)
                   end
               end
        @test num_edges(sim2, ET) == 1
    end

    # test that we can call it with a from value for which we did not
    # have added edges
    apply!(sim3, AgentState1, [], ET; add_existing = ET) do _, id, sim
        remove_edges!(sim, AgentID(0), id, ET)
    end
    if has_hint(sim, ET, :SingleEdge)
        @test num_edges(sim3, ET) == 100 # all 100 agents have 1 edge
    else
        @test num_edges(sim3, ET) == 9900 # all 100 agents have 99 edges
    end

    @test_throws AssertionError apply!(sim3, AgentState1, [], []) do _, id, sim
        remove_edges!(sim, AgentID(0), id, ET)
    end

    @test_throws AssertionError apply!(sim3, AgentState1, [], ET) do _, id, sim
        remove_edges!(sim, AgentID(0), id, ET)
    end
    
    
    finish_simulation!(sim)
    finish_simulation!(sim2)
    finish_simulation!(sim3)
end


@testset "remove edges" begin
    CurrentEdgeType = Nothing

    if CurrentEdgeType === Nothing
        for ET in [ MPIEdgeD, MPIEdgeS, MPIEdgeE, MPIEdgeI, MPIEdgeSE,
                 MPIEdgeSI, MPIEdgeEI, MPIEdgeSEI, MPIEdgeSTI, MPIEdgeSETI,
                 MPIEdgeT, MPIEdgeST ]
            testforedgetype_remove(ET)
        end
    else
        testforedgetype_remove(CurrentEdgeType)
    end

    if CurrentEdgeType === Nothing
        for ET in [ MPIEdgeD, MPIEdgeS, MPIEdgeE, MPIEdgeSE,
                 MPIEdgeT, MPIEdgeST ]
            testforedgetype_remove_from(ET)
        end
    else
        testforedgetype_remove_from(CurrentEdgeType)
    end
    
    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end
###
