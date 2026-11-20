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

@testset "Geometric registration smoke tests" begin
    types = ModelTypes() |> register_agenttype!(A) |> register_agenttype!(B)
    d = GeometricNeighborhood(space=space, posfunc=position, query_types=A, neighbor_types=[A,B], project=projection)
    @test register_geom_neighborhood!(types,:nearby,d) === types
    r = only(types.geom_neighborhoods)
    @test r.name === :nearby
    @test r.coordinate_type === Float64
    @test r.project_type === NamedTuple{(:x,),Tuple{Float64}}
    @test r.definition === d
    @test r.backend.leafsize == 25
    @test_throws AssertionError register_geom_neighborhood!(types,:nearby,d)
    @test length(types.geom_neighborhoods) == 1
    @test (types |> register_geom_neighborhood!(:second,d;backend=ReplicatedKDTree(leafsize=8))) === types
    @test types.geom_neighborhoods[2].backend.leafsize == 8
    identity_definition = GeometricNeighborhood(space=space,posfunc=position,query_types=A,neighbor_types=[A,B])
    @test_throws AssertionError register_geom_neighborhood!(types,:identity,identity_definition)
    @test length(types.geom_neighborhoods) == 2
    explicit = GeometricNeighborhood(space=space,posfunc=position,query_types=A,neighbor_types=B,project=projection,project_type=NamedTuple{(:x,),Tuple{Float64}})
    @test register_geom_neighborhood!(types,:explicit,explicit) === types
    missing = ModelTypes() |> register_agenttype!(A)
    @test_throws AssertionError register_geom_neighborhood!(missing,:missing,d)
    @test isempty(missing.geom_neighborhoods)
    gridtypes = ModelTypes() |> register_agenttype!(G)
    grid = GeometricNeighborhood(space=GridSpace([(0,10),(0,10)]),posfunc=position,query_types=G,neighbor_types=G)
    @test register_geom_neighborhood!(gridtypes,:grid,grid) === gridtypes
    @test only(gridtypes.geom_neighborhoods).coordinate_type === Int16
    @test only(gridtypes.geom_neighborhoods).project_type === G
end
