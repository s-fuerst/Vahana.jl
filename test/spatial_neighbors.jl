using Vahana, Test, StaticArrays

struct AgentWithPosFrom
    pos::SVector{2,Int64}
    age::Float64
end

struct AgentWithPosTo
    pos::SVector{2,Int64}
end

struct Agent3DFrom
    pos::SVector{3,Float64}
end

struct Agent3DTo
    pos::SVector{3,Float64}
end

struct Neighbor end

spatial_test_position(agent) = agent.pos
spatial_test_adult(agent) = agent.age >= 18

const spatial_test_grid_zero = GridSpace([(0, 4), (0, 5)]; periodic=true)
const spatial_test_grid = GridSpace([(1, 5), (1, 6)])
const spatial_test_grid_periodic = GridSpace([(1, 5), (1, 6)]; periodic=true)
const spatial_test_grid_periodic_x =
    GridSpace([(1, 5), (1, 6)]; periodic=(true, false))
const spatial_test_3d = ContinuousSpace([(1, 4), (3, 6), (4, 6)])
const spatial_test_unit_cube = [(0, 1), (0, 1), (0, 1)]
const spatial_test_cube_periodic_x =
    ContinuousSpace(spatial_test_unit_cube; periodic=(true, false, false))
const spatial_test_cube_periodic =
    ContinuousSpace(spatial_test_unit_cube; periodic=true)

function spatial_test_neighborhood(space, query_type, neighbor_type, radius;
                                   neighbor_filter=nothing)
    GeometricNeighborhood(
        space, spatial_test_position, query_type, neighbor_type;
        radius, neighbor_filter,
    )
end

spatial_test_value(a::AgentWithPosFrom) = a.age
spatial_test_value(::AgentWithPosTo) = -1.0

function spatial_test_mixed_neighborhood(neighbor_filter)
    types = [AgentWithPosFrom, AgentWithPosTo]
    GeometricNeighborhood(
        spatial_test_grid, spatial_test_position, types, types;
        radius=1.0, project=spatial_test_value, neighbor_filter,
    )
end

spatial_test_mixed_filter(a::AgentWithPosFrom) = a.age >= 18
spatial_test_mixed_filter(::AgentWithPosTo) = true

const spatial_model = ModelTypes() |>
    register_agenttype!(AgentWithPosFrom, :Immortal) |>
    register_agenttype!(AgentWithPosTo, :Immortal) |>
    register_agenttype!(Agent3DFrom, :Immortal) |>
    register_agenttype!(Agent3DTo) |>
    register_edgetype!(Neighbor) |>
    register_geom_neighborhood!(:grid_zero, spatial_test_neighborhood(
        spatial_test_grid_zero, AgentWithPosTo, AgentWithPosFrom, 1.0,
    )) |>
    register_geom_neighborhood!(:grid_near, spatial_test_neighborhood(
        spatial_test_grid, AgentWithPosTo, AgentWithPosFrom, 1.5,
    )) |>
    register_geom_neighborhood!(:grid_far, spatial_test_neighborhood(
        spatial_test_grid, AgentWithPosTo, AgentWithPosFrom, 2.0,
    )) |>
    register_geom_neighborhood!(:grid_adult, spatial_test_neighborhood(
        spatial_test_grid, AgentWithPosTo, AgentWithPosFrom, 2.0;
        neighbor_filter=spatial_test_adult,
    )) |>
    register_geom_neighborhood!(:grid_same_type, spatial_test_neighborhood(
        spatial_test_grid, AgentWithPosTo, AgentWithPosTo, 3.0,
    )) |>
    register_geom_neighborhood!(:grid_periodic, spatial_test_neighborhood(
        spatial_test_grid_periodic, AgentWithPosTo, AgentWithPosFrom, 1.0,
    )) |>
    register_geom_neighborhood!(:grid_periodic_x, spatial_test_neighborhood(
        spatial_test_grid_periodic_x, AgentWithPosTo, AgentWithPosFrom, 1.0,
    )) |>
    register_geom_neighborhood!(:continuous_3d, spatial_test_neighborhood(
        spatial_test_3d, Agent3DTo, Agent3DFrom, 1.5,
    )) |>
    register_geom_neighborhood!(:cube_periodic_x, spatial_test_neighborhood(
        spatial_test_cube_periodic_x, Agent3DTo, Agent3DTo, 0.1001,
    )) |>
    register_geom_neighborhood!(:cube_periodic, spatial_test_neighborhood(
        spatial_test_cube_periodic, Agent3DTo, Agent3DTo, 0.1001,
    )) |>
    register_geom_neighborhood!(:grid_from, spatial_test_neighborhood(
        spatial_test_grid, AgentWithPosFrom, AgentWithPosFrom, 3.0,
    )) |>
    register_geom_neighborhood!(:mixed_all, spatial_test_mixed_neighborhood(nothing)) |>
    register_geom_neighborhood!(:mixed_filtered, spatial_test_mixed_neighborhood(
        spatial_test_mixed_filter,
    )) |>
    create_model("Spatial_Neighbors")

function spatial_test_add_grid_agents!(sim, offset)
    positions_2d = ((0, 0), (4, 0), (0, 1), (0, 2), (3, 5))
    ages = (5.0, 20.0, 20.0, 5.0, 20.0)
    for (pos, age) in zip(positions_2d, ages)
        add_agent!(sim, AgentWithPosFrom(pos .+ offset, age))
    end

    targets = ((0, 0), (0, 0), (1, 1), (0, 4), (4, 5))
    map(pos -> add_agent!(sim, AgentWithPosTo(pos .+ offset)), targets)
end

function spatial_test_connect!(sim, query_type, neighbor_type, ::Val{name}) where {name}
    read = unique([query_type, neighbor_type])
    apply!(sim, query_type, read, Neighbor; neighborhoods=name) do _, id, sim
        for neighbor_id in geom_neighborids_iter(sim, id, name)
            add_edge!(sim, neighbor_id, id, Neighbor())
        end
        nothing
    end
end

function spatial_test_edge_counts(sim, idmap, target_ids)
    edges = all_edges(sim, Neighbor)

    counts = map(target_ids) do id
        # finish_init! distributes the graph created on rank 0.
        root_id = Vahana.agent_id(Vahana.type_nr(id), 0, Vahana.agent_nr(id))
        target_id = idmap[root_id]
        count(edge -> edge[1] == target_id, edges)
    end

    @test length(edges) == sum(counts)
    counts
end

@testset "Edges from geometric neighborhoods" begin
    sim = create_simulation(spatial_model)
    targets = spatial_test_add_grid_agents!(sim, 0)
    idmap = finish_init!(sim; return_idmapping=true)

    spatial_test_connect!(sim, AgentWithPosTo, AgentWithPosFrom, Val(:grid_zero))
    @test spatial_test_edge_counts(sim, idmap, targets) == (3, 3, 1, 0, 2)
    finish_simulation!(sim)

    sim = create_simulation(spatial_model)
    targets = spatial_test_add_grid_agents!(sim, 1)
    idmap = finish_init!(sim; return_idmapping=true)

    for (name, neighbor_type, expected) in (
        (:grid_near, AgentWithPosFrom, (2, 2, 3, 0, 1)),
        (:grid_far, AgentWithPosFrom, (3, 3, 3, 1, 1)),
        (:grid_adult, AgentWithPosFrom, (1, 1, 1, 0, 1)),
        (:grid_same_type, AgentWithPosTo, (2, 2, 2, 0, 0)),
        (:grid_periodic, AgentWithPosFrom, (3, 3, 1, 0, 2)),
        (:grid_periodic_x, AgentWithPosFrom, (3, 3, 1, 0, 1)),
    )
        spatial_test_connect!(sim, AgentWithPosTo, neighbor_type, Val(name))
        @test spatial_test_edge_counts(sim, idmap, targets) == expected
    end
    finish_simulation!(sim)

    sim = create_simulation(spatial_model)
    for pos in ((1.0, 3.0, 4.0), (1.0, 6.0, 4.0), (1.0, 3.0, 5.0),
                (1.0, 4.0, 6.0), (4.0, 6.0, 6.0))
        add_agent!(sim, Agent3DFrom(pos))
    end
    targets = map(
        pos -> add_agent!(sim, Agent3DTo(pos)),
        ((1.0, 3.0, 4.0), (4.0, 6.0, 4.0), (1.0, 4.0, 5.0),
         (4.0, 4.0, 6.0), (1.0, 6.0, 6.0)),
    )
    idmap = finish_init!(sim; return_idmapping=true)

    spatial_test_connect!(sim, Agent3DTo, Agent3DFrom, Val(:continuous_3d))
    @test spatial_test_edge_counts(sim, idmap, targets) == (2, 0, 3, 0, 0)
    finish_simulation!(sim)

    sim = create_simulation(spatial_model)
    targets = map(
        pos -> add_agent!(sim, Agent3DTo(pos)),
        ((0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.1, 0.1, 0.0),
         (0.0, 0.0, 0.1), (0.0, 0.0, 1.0), (1.0, 0.0, 1.0)),
    )
    idmap = finish_init!(sim; return_idmapping=true)

    spatial_test_connect!(sim, Agent3DTo, Agent3DTo, Val(:cube_periodic_x))
    @test spatial_test_edge_counts(sim, idmap, targets) == (2, 2, 0, 2, 1, 1)

    for _ in 1:2
        spatial_test_connect!(sim, Agent3DTo, Agent3DTo, Val(:cube_periodic))
        @test spatial_test_edge_counts(sim, idmap, targets) == (4, 4, 0, 4, 4, 4)
    end
    finish_simulation!(sim)
end

@testset "Geometric search in apply!" begin
    sim = create_simulation(spatial_model)
    spatial_test_add_grid_agents!(sim, 0)
    finish_init!(sim)

    apply!(sim, AgentWithPosTo, [AgentWithPosTo, AgentWithPosFrom], [];
           neighborhoods=:grid_zero) do agent, id, sim
        if agent.pos == SVector(0, 0)
            @test length(geom_neighborstates(sim, id, :grid_zero)) == 3
        elseif agent.pos == SVector(1, 1)
            @test count(_ -> true, geom_neighborstates_iter(sim, id, :grid_zero)) == 1
        elseif agent.pos == SVector(0, 4)
            @test isempty(geom_neighborstates(sim, id, :grid_zero))
        else
            @test agent.pos == SVector(4, 5)
            @test count(_ -> true, geom_neighborstates_iter(sim, id, :grid_zero)) == 2
        end
        nothing
    end
    finish_simulation!(sim)
end

spatial_test_search(f::F, sim, id) where {F} = f(sim, id, :grid_from)

function spatial_test_check_results(sim, id, expected)
    pairs = @inferred spatial_test_search(geom_neighbors, sim, id)
    ids = @inferred spatial_test_search(geom_neighborids, sim, id)
    states = @inferred spatial_test_search(geom_neighborstates, sim, id)

    pair_iter = @inferred spatial_test_search(geom_neighbors_iter, sim, id)
    id_iter = @inferred spatial_test_search(geom_neighborids_iter, sim, id)
    state_iter = @inferred spatial_test_search(geom_neighborstates_iter, sim, id)

    @test pairs isa Vector{Tuple{AgentID,AgentWithPosFrom}}
    @test ids isa Vector{AgentID}
    @test states isa Vector{AgentWithPosFrom}
    @test eltype(pair_iter) === Tuple{AgentID,AgentWithPosFrom}
    @test eltype(id_iter) === AgentID
    @test eltype(state_iter) === AgentWithPosFrom

    expected_ids = first.(expected)
    expected_states = last.(expected)
    by_age = a -> a.age

    @test sort(pairs; by=first) == sort(expected; by=first)
    @test sort(ids) == sort(expected_ids)
    @test sort(states; by=by_age) == sort(expected_states; by=by_age)
    @test sort(collect(pair_iter); by=first) == sort(expected; by=first)
    @test sort(collect(id_iter)) == sort(expected_ids)
    @test sort(collect(state_iter); by=by_age) == sort(expected_states; by=by_age)
end

@testset "Geometric search result forms" begin
    sim = create_simulation(spatial_model)
    same_position = AgentWithPosFrom((1, 1), 20.0)
    at_radius = AgentWithPosFrom((4, 1), 30.0)

    local_expected = Tuple{AgentID,AgentWithPosFrom}[]
    if mpi.rank == 0
        add_agent!(sim, AgentWithPosFrom((1, 1), 10.0)) # self is not a hit
        add_agent!(sim, AgentWithPosFrom((1, 6), 11.0)) # no hits
    end
    if mpi.rank == mpi.size - 1
        for state in (same_position, at_radius)
            id = add_agent!(sim, state)
            push!(local_expected, (id, state))
        end
        add_agent!(sim, AgentWithPosFrom((5, 1), 40.0)) # outside the radius
    end
    finish_init!(sim; distribute=false)
    expected = Vahana.join(local_expected)

    apply!(sim, AgentWithPosFrom, AgentWithPosFrom, []; neighborhoods=:grid_from) do a, id, sim
        if a.age == 10.0
            spatial_test_check_results(sim, id, expected)
        elseif a.age == 11.0
            spatial_test_check_results(sim, id, Tuple{AgentID,AgentWithPosFrom}[])
        end
        nothing
    end
    finish_simulation!(sim)
end

@testset "Geometric neighbors across type boundaries" begin
    sim = create_simulation(spatial_model)
    local_records = Tuple{AgentID,Float64}[]

    if mpi.rank == 0
        for (pos, age) in (((1, 1), 10.0), ((5, 6), 30.0), ((1, 1), 20.0))
            id = add_agent!(sim, AgentWithPosFrom(pos, age))
            push!(local_records, (id, age))
        end
    end
    if mpi.rank == mpi.size - 1
        id = add_agent!(sim, AgentWithPosTo((1, 1)))
        push!(local_records, (id, -1.0))
    end
    finish_init!(sim; distribute=false)
    records = Vahana.join(local_records)

    types = [AgentWithPosFrom, AgentWithPosTo]
    for name in (:mixed_all, :mixed_filtered)
        apply!(sim, types, types, []; neighborhoods=name) do agent, id, sim
            expected = Tuple{AgentID,Float64}[]
            if agent.pos == SVector(1, 1)
                for (neighbor_id, value) in records
                    in_range = value != 30.0
                    passes_filter = name == :mixed_all || value != 10.0
                    if neighbor_id != id && in_range && passes_filter
                        push!(expected, (neighbor_id, value))
                    end
                end
            end

            @test sort(geom_neighbors(sim, id, name); by=first) ==
                  sort(expected; by=first)
            @test sort(geom_neighborids(sim, id, name)) == sort(first.(expected))
            @test sort(geom_neighborstates(sim, id, name)) == sort(last.(expected))
            nothing
        end
    end
    finish_simulation!(sim)
end

struct IndependentSnapshotNeighbor
    pos::SVector{2,Int64}
    age::Float64
end

const spatial_independent_snapshot_model = ModelTypes() |>
    register_agenttype!(IndependentSnapshotNeighbor, :Immortal, :Independent) |>
    register_agenttype!(AgentWithPosTo, :Immortal) |>
    register_edgetype!(Neighbor) |>
    register_geom_neighborhood!(:grid_near, spatial_test_neighborhood(
        spatial_test_grid, AgentWithPosTo, IndependentSnapshotNeighbor, 1.5,
    )) |>
    create_model("Spatial_Independent_Snapshot")

@testset "Geometric snapshot lifetime: $mode" for (mode, model, neighbor_type) in (
    ("buffered", spatial_model, AgentWithPosFrom),
    ("independent", spatial_independent_snapshot_model, IndependentSnapshotNeighbor),
)
    sim = create_simulation(model)
    old_neighbor = neighbor_type((1, 1), 20.0)
    new_neighbor = neighbor_type((5, 6), 99.0)

    if mpi.rank == 0
        add_agent!(sim, old_neighbor)
    end
    if mpi.rank == mpi.size - 1
        add_agent!(sim, AgentWithPosTo(old_neighbor.pos))
        add_agent!(sim, AgentWithPosTo(new_neighbor.pos))
    end
    finish_init!(sim; distribute=false)

    for visible_neighbor in (old_neighbor, new_neighbor)
        apply!(sim, [neighbor_type, AgentWithPosTo],
               [neighbor_type, AgentWithPosTo], neighbor_type;
               neighborhoods=:grid_near) do a, id, sim
            if a isa neighbor_type
                new_neighbor
            else
                states = geom_neighborstates(sim, id, :grid_near)
                if a.pos == visible_neighbor.pos
                    @test states == [visible_neighbor]
                else
                    @test isempty(states)
                end
                nothing
            end
        end

        @test sim.geom_neighborhoods.grid_near.prepared === nothing
    end

    finish_simulation!(sim)
end
