using Vahana
using Test

struct AIndependent
    foo::Int64
end

struct ANotIndependent
    foo::Int64
end

struct AIndependentImmortal
    foo::Int64
end

struct AFooEdge
    foo::Int64
end

struct AEdge end

model = ModelTypes() |>
    register_agenttype!(AIndependent, :Independent) |>
    register_agenttype!(ANotIndependent) |>
    register_agenttype!(AIndependentImmortal, :Independent) |>
    register_edgetype!(AFooEdge) |>
    register_edgetype!(AEdge) |> 
    create_model("Test Independent")


@testset "independent" begin
    sim = create_simulation(model)
    simnot = create_simulation(model)
    simimmortal = create_simulation(model)

    ids = [ add_agent!(sim, AIndependent(i)) for i in 1:10 ]
    for from in ids
        for to in ids
            add_edge!(sim, from, to, AEdge())
        end
    end

    ids = [ add_agent!(simnot, ANotIndependent(i)) for i in 1:10 ]
    for from in ids
        for to in ids
            add_edge!(simnot, from, to, AEdge())
        end
    end

    ids = [ add_agent!(simimmortal, AIndependentImmortal(i)) for i in 1:10 ]
    for from in ids
        for to in ids
            add_edge!(simimmortal, from, to, AEdge())
        end
    end
    
    
    finish_init!(sim)
    finish_init!(simnot)
    finish_init!(simimmortal)

    @test num_agents(sim, AIndependent) == 10
    @test num_edges(sim, AEdge) == 10 * 10

    # remove agent i, all other add edges of type AFooEdge to the neighbors
    apply!(sim, AIndependent,
           [AIndependent, AEdge],
           [AIndependent, AFooEdge]) do state, id, sim
               if state.foo == 1
                   nothing
               else
                   foreach(neighborids(sim, id, AEdge)) do nid
                       add_edge!(sim, nid, id, AFooEdge(state.foo))
                       add_edge!(sim, id, nid, AFooEdge(state.foo))
                   end
                   state
               end
           end

    apply!(simnot, ANotIndependent,
           [ANotIndependent, AEdge],
           [ANotIndependent, AFooEdge]) do state, id, sim
               if state.foo == 1
                   nothing
               else
                   foreach(neighborids(sim, id, AEdge)) do nid
                       add_edge!(sim, nid, id, AFooEdge(state.foo))
                       add_edge!(sim, id, nid, AFooEdge(state.foo))
                   end
                   state
               end
           end

    apply!(simimmortal, AIndependentImmortal,
           [AIndependentImmortal, AEdge],
           [AIndependentImmortal, AFooEdge]) do state, id, sim
               if state.foo == 1
                   nothing
               else
                   foreach(neighborids(sim, id, AEdge)) do nid
                       add_edge!(sim, nid, id, AFooEdge(state.foo))
                       add_edge!(sim, id, nid, AFooEdge(state.foo))
                   end
                   state
               end
           end

    @test num_agents(sim, AIndependent) == num_agents(simnot, ANotIndependent)
    @test num_edges(sim, AEdge) == num_edges(simnot, AEdge)
    @test num_edges(sim, AFooEdge) == num_edges(simnot, AFooEdge)
    @test num_agents(simimmortal, AIndependentImmortal) == num_agents(simnot, ANotIndependent)
    @test num_edges(simimmortal, AEdge) == num_edges(simnot, AEdge)
    @test num_edges(simimmortal, AFooEdge) == num_edges(simnot, AFooEdge)

    
    apply!(sim,
           AIndependent,
           [AIndependent, AEdge],
           [AIndependent, AFooEdge]) do state, id, sim
               if state.foo == 2
                   nothing
               else
                   foreach(neighborids(sim, id, AEdge)) do nid
                       add_edge!(sim, nid, id, AFooEdge(state.foo))
                       add_edge!(sim, id, nid, AFooEdge(state.foo))
                   end
                   state
               end
           end

    apply!(simnot,
           ANotIndependent,
           [ANotIndependent, AEdge],
           [ANotIndependent, AFooEdge]) do state, id, sim
               if state.foo == 2
                   nothing
               else
                   foreach(neighborids(sim, id, AEdge)) do nid
                       add_edge!(sim, nid, id, AFooEdge(state.foo))
                       add_edge!(sim, id, nid, AFooEdge(state.foo))
                   end
                   state
               end
           end

    apply!(simimmortal,
           AIndependentImmortal,
           [AIndependentImmortal, AEdge],
           [AIndependentImmortal, AFooEdge]) do state, id, sim
               if state.foo == 2
                   nothing
               else
                   foreach(neighborids(sim, id, AEdge)) do nid
                       add_edge!(sim, nid, id, AFooEdge(state.foo))
                       add_edge!(sim, id, nid, AFooEdge(state.foo))
                   end
                   state
               end
           end
    
    
    @test num_agents(sim, AIndependent) == num_agents(simnot, ANotIndependent)
    @test num_edges(sim, AEdge) == num_edges(sim, AEdge)
    @test num_edges(sim, AFooEdge) == num_edges(sim, AFooEdge)
    @test num_agents(simimmortal, AIndependentImmortal) == num_agents(simnot, ANotIndependent)
    @test num_edges(simimmortal, AEdge) == num_edges(simnot, AEdge)
    @test num_edges(simimmortal, AFooEdge) == num_edges(simnot, AFooEdge)

    apply!(sim,
           AIndependent,
           [AIndependent, AEdge],
           [AIndependent, AFooEdge];
           add_existing = AFooEdge) do state, id, sim
               nid = add_agent!(sim, AIndependent(state.foo+10))
               add_edge!(sim, nid, id, AFooEdge(nid))
               add_edge!(sim, id, nid, AFooEdge(nid))
               nid = add_agent!(sim, AIndependent(state.foo+20))
               add_edge!(sim, nid, id, AFooEdge(nid))
               add_edge!(sim, id, nid, AFooEdge(nid))
               state
           end

    apply!(simnot,
           ANotIndependent,
           [ANotIndependent, AEdge],
           [ANotIndependent, AFooEdge];
           add_existing = AFooEdge) do state, id, sim
               nid = add_agent!(sim, ANotIndependent(state.foo+10))
               add_edge!(sim, nid, id, AFooEdge(nid))
               add_edge!(sim, id, nid, AFooEdge(nid))
               nid = add_agent!(sim, ANotIndependent(state.foo+20))
               add_edge!(sim, nid, id, AFooEdge(nid))
               add_edge!(sim, id, nid, AFooEdge(nid))
               state
           end

    apply!(simimmortal,
           AIndependentImmortal,
           [AIndependentImmortal, AEdge],
           [AIndependentImmortal, AFooEdge];
           add_existing = AFooEdge) do state, id, sim
               nid = add_agent!(sim, AIndependentImmortal(state.foo+10))
               add_edge!(sim, nid, id, AFooEdge(nid))
               add_edge!(sim, id, nid, AFooEdge(nid))
               nid = add_agent!(sim, AIndependentImmortal(state.foo+20))
               add_edge!(sim, nid, id, AFooEdge(nid))
               add_edge!(sim, id, nid, AFooEdge(nid))
               state
           end
    

    @test num_agents(sim, AIndependent) == num_agents(simnot, ANotIndependent)
    @test num_edges(sim, AEdge) == num_edges(sim, AEdge)
    @test num_edges(sim, AFooEdge) == num_edges(sim, AFooEdge)
    @test num_agents(simimmortal, AIndependentImmortal) == num_agents(simnot, ANotIndependent)
    @test num_edges(simimmortal, AEdge) == num_edges(simnot, AEdge)
    @test num_edges(simimmortal, AFooEdge) == num_edges(simnot, AFooEdge)

    @infiltrate
    
    finish_simulation!(sim)
    finish_simulation!(simnot)
    finish_simulation!(simimmortal)

    sleep(mpi.rank * 0.05)
end

