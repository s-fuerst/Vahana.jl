@testset "Aggregate" begin
    sim = Simulation("Aggregate", nothing, nothing)

    add_agenttype!(sim, Person)
    pids = add_agents!(sim, [ Person(i) for i in 1:10 ])

    add_edgetype!(sim, FooEdgeState)
    for i in pids
        add_edge!(sim, i, last(pids), FooEdgeState(1))
    end
    finish_init!(sim)
    
    @test aggregate(sim, Person, p -> p.foo, +) == sum(1:10)
    @test aggregate(sim, FooEdgeState, e -> e.foo, +) == 10
end
