struct GridA 
    pos::Tuple{Int64, Int64}
    active::Bool
end

struct GridE end

struct Position 
    sum::Int64
end

struct MovingAgent 
    value::Int64
end

struct OnPosition  end

raster_model = ModelTypes() |>
    register_agenttype!(GridA) |>
    register_edgetype!(GridE) |>
    register_agenttype!(Position) |>
    register_agenttype!(MovingAgent) |>
    register_edgetype!(OnPosition) |>
    construct_model("Raster_Test")


@testset "Raster" begin
    # calculate the sum of all ids
    function diffuse(a, id, sim)
        GridA(a.pos, a.active ||
            mapreduce(a -> a.active, |, neighborstates(sim, id, GridE, GridA)))
    end

    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (10,8),
                p -> (p[1] == 10 && p[2] == 8) ? GridA(p, true) : GridA(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              _ -> GridE())

    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, GridA) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 25

    raster = calc_raster(sim, :grid, c -> c.active, GridA)
    @test raster[1,3] == false
    @test raster[1,1] == true
    @test raster[4,4] == false
    
    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                (7,9),
                p -> (p[1] == 3 && p[2] == 5) ? GridA(p, true) : GridA(p, false),
                GridE()
                )

    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, GridA) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 25
    # @test aggregate(sim, GridA, a -> a.active, +) == 1
    # apply_transition!(sim, diffuse, [GridA], [GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 9
    # apply_transition!(sim, diffuse, [GridA], [GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 25

    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                (17,6),
                p -> (p[1] == 9 && p[2] == 1) ? GridA(p, true) : GridA(p, false),
                GridE()
                )

    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, GridA) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 25
    # @test aggregate(sim, GridA, a -> a.active, +) == 1
    # apply_transition!(sim, diffuse, [GridA], [GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 9
    # apply_transition!(sim, diffuse, [GridA], [GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 25

    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                (6,7),
                p -> (p[1] == 1 && p[2] == 1) ? GridA(p, true) : GridA(p, false),
                GridE()
                )

    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, GridA) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 25
    
    
end

@testset "Raster_NodeID" begin
    function sum_on_pos(cell::Position, id, sim)
        nstates =neighborstates_flexible(sim, id, OnPosition)
        if !isnothing(nstates)
            Position(mapreduce(c -> c.value, +, nstates))
        else
            Position(0)
        end
    end

    function value_on_pos(a::MovingAgent, id, sim)
        value = first(neighborstates_flexible(sim, id, OnPosition)).sum
        move_to!(sim, :raster, id, (value, value), OnPosition)
        MovingAgent(value)
    end

    sim = new_simulation(raster_model, "Raster NodeID", nothing)

    add_raster!(sim,
                (10,10),
                _ -> Position(0),
                GridE();
                name = :raster
                )

    p1 = add_agent!(sim, MovingAgent(1))
    p2 = add_agent!(sim, MovingAgent(2))
    p3 = add_agent!(sim, MovingAgent(3))

    move_to!(sim, :raster, p1, (1,1), OnPosition)
    move_to!(sim, :raster, p2, (2,2), OnPosition)
    move_to!(sim, :raster, p3, (2,2), OnPosition)
    
    # add_edge!(sim, p1, raster_nodeid(sim, :raster, (1,1)), OnPosition)
    # add_edge!(sim, raster_nodeid(sim, :raster, (1,1)), p1, OnPosition)
    
    finish_init!(sim)

    apply_transition!(sim, sum_on_pos, [ Position ], [ OnPosition ], [])
    raster = calc_raster_flexible(sim, :raster, c -> c.sum)
    @test raster[1,1] == 1
    @test raster[1,2] == 0
    @test raster[2,2] == 5
    @test raster[2,3] == 0

    apply_transition!(sim,
                      value_on_pos,
                      [ MovingAgent ],
                      [ OnPosition ],
                      [ OnPosition ])
    apply_transition!(sim, sum_on_pos, [ Position ], [ OnPosition ], [])
    raster = calc_raster_flexible(sim, :raster, c -> c.sum)
    @test raster[1,1] == 1
    @test raster[1,2] == 0
    @test raster[2,2] == 0
    @test raster[5,5] == 10
end

