@testset "Globals" begin
    mutable struct TestGlobals
        foo::Float64
        bar::Vector{Int64}
    end
    
    sim = Simulation("Globals Test", nothing, TestGlobals(0,Vector{Int64}()))
    setglobal!(sim, :foo, 1.1)
    pushglobal!(sim, :bar, 1)
    pushglobal!(sim, :bar, 2)

    @test getglobal(sim, :foo) == 1.1
    @test getglobal(sim, :bar) == [1, 2]
end
