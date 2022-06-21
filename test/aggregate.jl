@testset "Aggregate" begin
    sim = construct(model, "Aggregate", nothing, nothing)

    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
    finish_init!(sim)
    
    @test aggregate(sim, a -> a.foo, +, ADict) == 6
    @test aggregate(sim, a -> a.foo, +, AVec) == sum(1:10)
    @test aggregate(sim, e -> e.foo, +, ESDict) == 10
end
