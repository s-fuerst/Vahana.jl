import Vahana: @onrankof, disable_transition_checks

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
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 25

    raster = calc_rasterstate(sim, :grid, c -> c.active, Bool, GridA)
    @test raster[1,3] == false
    @test raster[1,1] == true
    @test raster[4,4] == false

    raster = calc_raster(sim, :grid, id -> agentstate(sim, id, GridA).active, Bool, [ GridA ])
    @test raster[1,3] == false
    @test raster[1,1] == true
    @test raster[4,4] == false

    finish_simulation!(sim)
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
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 25
    # @test aggregate(sim, GridA, a -> a.active, +) == 1
    # apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 9
    # apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 25

    finish_simulation!(sim)
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
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 25
    # @test aggregate(sim, GridA, a -> a.active, +) == 1
    # apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 9
    # apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [])
    # @test aggregate(sim, GridA, a -> a.active, +) == 25

    finish_simulation!(sim)

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
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 9
    apply_transition!(sim, diffuse, [GridA], [GridA, GridE], [GridA])
    @test aggregate(sim, a -> a.active, +, GridA) == 25

    ### 3D 
    function diffuse3D(a, id, sim)
        Grid3D(a.pos, a.active ||
            mapreduce(a -> a.active, |, neighborstates(sim, id, GridE, Grid3D)))
    end

    finish_simulation!(sim)

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
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 5*5*5
    
    ### non-periodic 
    finish_simulation!(sim)

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
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 2*2*2
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3

    ### distance 2 
    finish_simulation!(sim)

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
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 5*5*5
    finish_simulation!(sim)

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
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 3*3*3-8
    finish_simulation!(sim)

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
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 7

    finish_simulation!(sim)

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
    apply_transition!(sim, diffuse3D, [Grid3D], [Grid3D, GridE], [Grid3D])
    @test aggregate(sim, a -> a.active, +, Grid3D) == 10
    
    finish_simulation!(sim)

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end

@testset "Raster_NodeID" begin
    # we have a raster with nodes of type Position and agents of type
    # MovingAgent that are connected to this raster via edges of
    # type OnPosition

    # sum the value of all agents that are on the cell position
    function sum_on_pos(::Val{Position}, id, sim)
        nstates = neighborstates_flexible(sim, id, OnPosition)
        if !isnothing(nstates)
            Position(mapreduce(c -> c.value, +, nstates))
        else
            Position(0)
        end
    end

    # this transition function moves the agent to the new pos, wherby the new
    # pos is determined by the state of the current pos
    function value_on_pos(::Val{MovingAgent}, id, sim)
        # sum is not a sum operator, but a field of the agentstate of Position
        value = first(neighborstates_flexible(sim, id, OnPosition)).sum
        move_to!(sim, :raster, id, (value, value), OnPosition(), OnPosition())
        MovingAgent(value)
    end

    sim = new_simulation(raster_model, nothing, nothing)

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

    apply_transition!(sim, sum_on_pos, [ Position ], [ MovingAgent, OnPosition ], [ Position ])
    raster = calc_rasterstate(sim, :raster, c -> c.sum, Int64, Position)
    @test raster[1,1] == 1
    @test raster[1,2] == 0
    @test raster[2,2] == 5
    @test raster[2,3] == 0

    apply_transition!(sim,
                      value_on_pos,
                      [ MovingAgent ],
                      [ Position, OnPosition ],
                      [ OnPosition, MovingAgent ])
    apply_transition!(sim, sum_on_pos, [ Position ], [ MovingAgent, OnPosition ], [ Position ])
    raster = calc_rasterstate(sim, :raster, c -> c.sum, Int64, Position)
    @test raster[1,1] == 1
    @test raster[1,2] == 0
    @test raster[2,2] == 0
    @test raster[5,5] == 10
    finish_simulation!(sim)

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end

@testset "MoveTo_Dist" begin
    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :raster,
                (10,10),
                _ -> Position(0))

    
    p1 = add_agent!(sim, MovingAgent(1))
    p2 = add_agent!(sim, MovingAgent(1))
    p3 = add_agent!(sim, MovingAgent(1))

    move_to!(sim, :raster, p1, (4,4), OnPosition(), nothing;
             distance = 2)
    move_to!(sim, :raster, p2, (4,4), OnPosition(), nothing;
             distance = 2, metric = :manhatten)
    move_to!(sim, :raster, p3, (4,4), OnPosition(), nothing;
             distance = 2.5, metric = :euclidean)

    idmapping = finish_init!(sim, return_idmapping = true)

    p1 = Vahana.updateids(idmapping, p1)
    p2 = Vahana.updateids(idmapping, p2)
    p3 = Vahana.updateids(idmapping, p3)
    disable_transition_checks(true)
    @onrankof p1 @test num_neighbors(sim, p1, OnPosition) == 25
    @onrankof p2 @test num_neighbors(sim, p2, OnPosition) == 13
    @onrankof p3 @test num_neighbors(sim, p3, OnPosition) == 21
    disable_transition_checks(false)
    finish_simulation!(sim)

    ######################################## 4D
    
    sim = new_simulation(raster_model, nothing, nothing)

    add_raster!(sim,
                :raster,
                (10,10,10,10),
                _ -> Position(0))

    
    p1 = add_agent!(sim, MovingAgent(1))
    p2 = add_agent!(sim, MovingAgent(1))

    move_to!(sim, :raster, p1, (4,4,4,4), OnPosition(), nothing;
             distance = 1)
    move_to!(sim, :raster, p2, (4,4,4,4), OnPosition(), nothing;
             distance = 1, metric = :manhatten)

    idmapping = finish_init!(sim, return_idmapping = true)
    p1 = Vahana.updateids(idmapping, p1)
    p2 = Vahana.updateids(idmapping, p2)

    disable_transition_checks(true)
    @onrankof p1 @test num_neighbors(sim, p1, OnPosition) == 3*3*3*3
    @onrankof p2 @test num_neighbors(sim, p2, OnPosition) == 1+4*2
    disable_transition_checks(false)
    finish_simulation!(sim)

    # this hack should help that the output is not scrambled
    sleep(mpi.rank * 0.05)
end

