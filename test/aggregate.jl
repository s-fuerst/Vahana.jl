@testset "Aggregate" begin
    sim = construct(model, "Aggregate", nothing, nothing)

    (a1id, a2id, a3id, avids, avfids) = add_example_network!(sim)
    
    finish_init!(sim)
    
    @test aggregate(sim, Val(ADict), a -> a.foo, +) == 6
    @test aggregate(sim, Val(AVec), a -> a.foo, +) == sum(1:10)
    @test aggregate(sim, Val(ESDict), e -> e.foo, +) == 10
end
