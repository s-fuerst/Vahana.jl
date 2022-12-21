import Vahana.@onrankof
import Vahana.@rootonly
import Vahana.disable_transition_checks

struct DAgent end
struct DAgentRemove end
struct DEdgeState state::Int64 end
struct DEdge end
struct DSingleEdge end
struct DEdgeST end # ST = SingleAgentType

count_num_edges(sim, E) = sum(Vahana.join([ num_edges(sim, E) +
    num_edges(sim, DEdgeState) ]))

model = ModelTypes() |>
    register_agenttype!(DAgent) |>
    register_agenttype!(DAgentRemove) |>
    register_edgetype!(DEdge) |>
    register_edgetype!(DEdgeState) |>
    register_edgetype!(DSingleEdge, :SingleEdge) |>
    register_edgetype!(DEdgeST, :SingleAgentType; to_agenttype = DAgent) |>
    construct_model("remove_agents") 

@testset "Dying_Agents" begin
    function test_edgetype(E)
        sim = new_simulation(model)
        
        ids = add_agents!(sim, [ DAgent() for _ in 1:(mpi.size * 3)])

        rids = add_agents!(sim, [ DAgentRemove() for _ in 1:mpi.size ])

        # we create a network where with an edge for each agent to ids[2]
        foreach(id -> add_edge!(sim, id, ids[2], E()), ids)
        foreach(id -> add_edge!(sim, id, ids[2], DEdgeState(0)), rids)

        # and a single edge from ids[1] to ids[3]
        add_edge!(sim, ids[2], ids[3], E())
        
        finish_init!(sim; partition_algo = :EqualAgentNumbers)

        @test count_num_edges(sim, E) == (mpi.size * 4) + 1

        # we remove all ADefault edges, that should also remove the ESDict edges
        apply_transition!(sim,
                          [ DAgentRemove ],
                          [],
                          [ DAgentRemove ]) do _,_,_
                              nothing
                          end

        @test count_num_edges(sim, E) == (mpi.size * 3) + 1

        apply_transition!(sim, [ DAgent ], [ DAgent, E ], [ DAgent ]) do state, id, sim
            if num_neighbors(sim, id, E) == 0
                nothing
            else
                state
            end
        end

        # the remaining edges are ids[3]->ids[2],rids[3]->ids[2],ids[2]->ids[3]
        @test count_num_edges(sim, E) == 3
        
        finish_simulation!(sim)
    end

    test_edgetype(DEdge)
    test_edgetype(DEdgeST)
end
