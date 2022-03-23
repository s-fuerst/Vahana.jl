@testset "Grid" begin

    struct GridA <: Agent
        pos::Tuple{Int64, Int64}
        active::Bool
    end

    struct GridE <: EdgeState end

    # calculate the sum of all ids
    function diffuse(a, id, sim)
        GridA(a.pos, a.active ||
            mapreduce(a -> a.active, |, neighborstates(sim, id, GridE)))
    end

    sim = Simulation("Test Grid right botton", nothing, nothing)

    add_agenttype!(sim, GridA)
    add_edgetype!(sim, GridE)

    add_grid!(sim,
              (10,8),
              p -> (p[1] == 10 && p[2] == 8) ? GridA(p, true) : GridA(p, false),
              GridE()
              )

    finish_init!(sim)

    @test aggregate(sim, GridA, a -> a.active, +) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 25


    sim = Simulation("Test Grid center", nothing, nothing)

    add_agenttype!(sim, GridA)
    add_edgetype!(sim, GridE)

    add_grid!(sim,
              (7,9),
              p -> (p[1] == 3 && p[2] == 5) ? GridA(p, true) : GridA(p, false),
              GridE()
              )

    finish_init!(sim)

    @test aggregate(sim, GridA, a -> a.active, +) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 25


    sim = Simulation("Test Grid top center", nothing, nothing)

    add_agenttype!(sim, GridA)
    add_edgetype!(sim, GridE)

    add_grid!(sim,
              (17,6),
              p -> (p[1] == 9 && p[2] == 1) ? GridA(p, true) : GridA(p, false),
              GridE()
              )

    finish_init!(sim)

    @test aggregate(sim, GridA, a -> a.active, +) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 25

    sim = Simulation("Test Grid top left", nothing, nothing)

    add_agenttype!(sim, GridA)
    add_edgetype!(sim, GridE)

    add_grid!(sim,
              (6,7),
              p -> (p[1] == 1 && p[2] == 1) ? GridA(p, true) : GridA(p, false),
              GridE()
              )

    finish_init!(sim)

    @test aggregate(sim, GridA, a -> a.active, +) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, GridA, a -> a.active, +) == 25
    
    
end
