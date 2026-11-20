using Vahana, StaticArrays, Test

struct A
    pos::SVector{2,Float32}
end

struct B
    pos::Tuple{Float64,Float64}
end

struct G
    pos::Tuple{Int8,Int16}
end

position(a::Union{A,B,G}) = a.pos
projection(a::Union{A,B}) = (; x=Float64(a.pos[1]))

space = ContinuousSpace([(0,10),(0,10)])

@testset "Geometric registration" begin
    types = ModelTypes() |> register_agenttype!(A) |> register_agenttype!(B)
    d = GeometricNeighborhood(space, position, A, [A, B]; project=projection)
    @test register_geom_neighborhood!(types, :nearby, d) === types
    r = only(types.geom_neighborhoods)
    # A and B use different position types, but projection gives both the same result type.
    @test r.coordinate_type === Float64
    @test r.project_type === NamedTuple{(:x,),Tuple{Float64}}
    @test_throws AssertionError register_geom_neighborhood!(types, :nearby, d)

    @test (types |> register_geom_neighborhood!(:second, d; backend=ReplicatedKDTree(leafsize=8))) === types
    @test last(types.geom_neighborhoods).backend.leafsize == 8

    # The default identity projection returns A for A and B for B, not one common type.
    identity_definition = GeometricNeighborhood(space, position, A, [A, B])
    @test_throws AssertionError register_geom_neighborhood!(types, :identity, identity_definition)

    # Hide the return type from inference to exercise the explicit project_type path.
    explicit = GeometricNeighborhood(
        space, position, A, B;
        project=a -> Base.inferencebarrier(projection(a)),
        project_type=NamedTuple{(:x,),Tuple{Float64}},
    )
    register_geom_neighborhood!(types, :explicit, explicit)
    @test last(types.geom_neighborhoods).project_type === NamedTuple{(:x,),Tuple{Float64}}

    missing = ModelTypes() |> register_agenttype!(A)
    @test_throws AssertionError register_geom_neighborhood!(missing, :missing, d)

    gridtypes = ModelTypes() |> register_agenttype!(G)
    grid = GeometricNeighborhood(GridSpace([(0,10),(0,10)]), position, G, G)
    register_geom_neighborhood!(gridtypes, :grid, grid)
    # The coordinate type comes from the mixed Int8/Int16 position, not the grid bounds.
    @test only(gridtypes.geom_neighborhoods).coordinate_type === Int16
    @test only(gridtypes.geom_neighborhoods).project_type === G
end

struct GeomTestEdge
    value::Int
end

# create_simulation currently expects edge storage methods to have been generated.
geom_test_types() = ModelTypes() |> register_agenttype!(A) |> register_edgetype!(GeomTestEdge)

const geom_model = geom_test_types() |>
    register_geom_neighborhood!(:nearby, GeometricNeighborhood(space, position, A, A)) |>
    register_geom_neighborhood!(
        :projected, GeometricNeighborhood(space, position, A, A; project=projection),
    ) |>
    create_model("GeomTestModel")
const geom_empty_model = create_model(geom_test_types(), "GeomTestEmptyModel")

# Put the literal lookup inside the function so inference sees the field name.
geom_test_nearby(sim) = sim.geom_neighborhoods.nearby

@testset "Generated geometric neighborhood fields" begin
    sim = create_simulation(geom_model)
    other = create_simulation(geom_model)
    empty_sim = create_simulation(geom_empty_model)

    @test fieldtype(typeof(sim), :geom_neighborhoods) === typeof(sim.geom_neighborhoods)
    nearby = @inferred geom_test_nearby(sim)
    @test nearby !== other.geom_neighborhoods.nearby
    @test nearby.prepared === nothing
    @test other.geom_neighborhoods.nearby.prepared === nothing
    @test isempty(propertynames(empty_sim.geom_neighborhoods))
end

@testset "Copying geometric simulations" begin
    sim = create_simulation(geom_model)
    add_agent!(sim, A(SVector(1f0, 2f0)))
    finish_init!(sim)

    copied = copy_simulation(sim)
    @test copied.geom_neighborhoods.nearby !== sim.geom_neighborhoods.nearby
    @test copied.geom_neighborhoods.nearby.prepared === nothing
    finish_simulation!(copied)
    finish_simulation!(sim)

    # Even without registered neighborhoods, copying during a transition is invalid.
    empty_sim = create_simulation(geom_empty_model)
    add_agent!(empty_sim, A(SVector(1f0, 2f0)))
    finish_init!(empty_sim)
    apply!(empty_sim, A, A, A) do state, _, current_sim
        @test_throws ErrorException("Cannot copy a simulation during a transition.") copy_simulation(current_sim)
        state
    end
    finish_simulation!(empty_sim)
end
