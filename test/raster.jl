struct GridA 
    pos::Tuple{Int64, Int64}
    active::Bool
end

struct Grid3D 
    pos::Tuple{Int64, Int64, Int64}
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
    register_agenttype!(Grid3D) |> 
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
                              (_,_) -> GridE())

    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, GridA) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 25

    raster = calc_rasterstate(sim, :grid, c -> c.active, GridA)
    @test raster[1,3] == false
    @test raster[1,1] == true
    @test raster[4,4] == false
    
    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (7,9),
                p -> (p[1] == 3 && p[2] == 5) ? GridA(p, true) : GridA(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE())
    
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
                :grid,
                (17,6),
                p -> (p[1] == 9 && p[2] == 1) ? GridA(p, true) : GridA(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE())
    
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
                :grid,
                (6,7),
                p -> (p[1] == 1 && p[2] == 1) ? GridA(p, true) : GridA(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE())
    
    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, GridA) == 1
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridE], [])
    @test aggregate(sim, a -> a.active, +, GridA) == 25

    ### 3D 
    function diffuse3D(a, id, sim)
        Grid3D(a.pos, a.active ||
            mapreduce(a -> a.active, |, neighborstates(sim, id, GridE, Grid3D)))
    end
    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (6,7,6),
                p -> (p[1] == 1 && p[2] == 1 && p[3] == 1) ?
                    Grid3D(p, true) : Grid3D(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE())
    
    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, Grid3D) == 1
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 5*5*5
    
    ### non-periodic 

    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (6,7,6),
                p -> (p[1] == 1 && p[2] == 1 && p[3] == 1) ?
                Grid3D(p, true) : Grid3D(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE(),
                              periodic = false)
    
    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, Grid3D) == 1
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 2*2*2
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3

    ### distance 2 

    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (6,7,6),
                p -> (p[1] == 1 && p[2] == 1 && p[3] == 1) ?
                Grid3D(p, true) : Grid3D(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE(),
                              periodic = false,
                              distance = 2)
    
    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, Grid3D) == 1
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 5*5*5

    ### euclidean
    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (6,7,6),
                p -> (p[1] == 1 && p[2] == 1 && p[3] == 1) ?
                Grid3D(p, true) : Grid3D(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE();
                              distance = 1.5,
                              metric = :euclidean)
    
    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, Grid3D) == 1
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3-8

    ### manhatten
    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (6,7,6),
                p -> (p[1] == 1 && p[2] == 1 && p[3] == 1) ?
                Grid3D(p, true) : Grid3D(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE();
                              distance = 1.5,
                              metric = :manhatten)
    
    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, Grid3D) == 1
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 7


    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :grid,
                (6,7,6),
                p -> (p[1] == 1 && p[2] == 1 && p[3] == 1) ?
                Grid3D(p, true) : Grid3D(p, false))

    connect_raster_neighbors!(sim,
                              :grid,
                              (_,_) -> GridE();
                              periodic = false,
                              distance = 2,
                              metric = :manhatten)
    
    finish_init!(sim)

    @test aggregate(sim, a -> a.active, +, Grid3D) == 1
    apply_transition!(sim, diffuse3D, [Grid3D], [GridE], [])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 10
    
    
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
        move_to!(sim, :raster, id, (value, value), OnPosition(), OnPosition())
        MovingAgent(value)
    end

    sim = new_simulation(raster_model, "Raster NodeID", nothing)

    add_raster!(sim,
                :raster,
                (10,10),
                _ -> Position(0))

    connect_raster_neighbors!(sim,
                              :raster,
                              (_,_) -> GridE())

    p1 = add_agent!(sim, MovingAgent(1))
    p2 = add_agent!(sim, MovingAgent(2))
    p3 = add_agent!(sim, MovingAgent(3))

    move_to!(sim, :raster, p1, (1,1), OnPosition(), OnPosition())
    move_to!(sim, :raster, p2, (2,2), OnPosition(), OnPosition())
    move_to!(sim, :raster, p3, (2,2), OnPosition(), OnPosition())
    
    finish_init!(sim)

    apply_transition!(sim, sum_on_pos, [ Position ], [ OnPosition ], [])
    raster = calc_rasterstate(sim, :raster, c -> c.sum, Position)
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
    raster = calc_rasterstate(sim, :raster, c -> c.sum, Position)
    @test raster[1,1] == 1
    @test raster[1,2] == 0
    @test raster[2,2] == 0
    @test raster[5,5] == 10
end

