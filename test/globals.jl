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

    # testing register_global!
    mt = ModelTypes() |>
        register_global!(:test, Vector{Int64}()) |>
        create_model("Test_register_global!")

    sim1 = create_simulation(mt)
    finish_init!(sim1)
    sim2 = create_simulation(mt)
    finish_init!(sim2)

    push_global!(sim1, :test, 3)
    get_global(sim2, :test)
end
