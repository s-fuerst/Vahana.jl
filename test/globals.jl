@testset "Globals" begin
    mutable struct TestGlobals
        foo::Float64
        bar::Vector{Int64}
    end

    sim = create_simulation(model, nothing, TestGlobals(0,Vector{Int64}()))

    set_global!(sim, :foo, 1.1)
    push_global!(sim, :bar, 1)
    push_global!(sim, :bar, 2)

    @test get_global(sim, :foo) == 1.1
    @test get_global(sim, :bar) == [1, 2]

    finish_simulation!(sim)
end
