using Test
using Vahana
using MPI

enable_asserts(true)
suppress_warnings(true)

struct A1 x::Int64 end
struct A2 x::Int64 end
struct E1 end

# named transition: the caller label must be the exact function name
function named_trans(state, id, sim)
    A1(state.x + 1)
end

# dispatching transition for the multi-type section
function multi_trans(state, id, sim)
    if state isa A1
        A1(state.x + 1)
    else
        A2(state.x + 1)
    end
end

@testset "instrumentation (multi-rank)" begin
    model = ModelTypes() |>
        register_agenttype!(A1) |>
        register_edgetype!(E1) |>
        create_model("Instr MPI")

    sim = create_simulation(model)
    add_agents!(sim, [A1(i) for i in 1:100])
    finish_init!(sim)

    # 1) off state: collective apply! records nothing on any rank
    apply!(sim, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    @test isempty(sim.instrumentation.events)

    # 2) collective toggle on: every rank records its own events
    enable_instrumentation!(true)
    apply!(sim, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    apply!(sim, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])

    ev = sim.instrumentation.events
    @test !isempty(ev)
    @test length(filter(e -> e.label == :apply_total, ev)) == 2
    @test length(filter(e -> e.label == :barrier_pre_apply, ev)) == 2
    @test length(filter(e -> e.label == :barrier_post_apply, ev)) == 2
    @test length(filter(e -> e.label == :transition, ev)) == 2
    @test isempty(sim.instrumentation.stack)

    # per-rank invariants: one :apply_total per record, caller/context set,
    # consistent transition_nr, nesting inside the record interval
    tnrs = sort(collect(unique(e.transition_nr for e in ev)))
    @test length(tnrs) == 2
    for rec_tnr in tnrs
        rec = filter(e -> e.transition_nr == rec_tnr, ev)
        top = only(filter(e -> e.label == :apply_total, rec))
        @test top.caller !== nothing
        @test top.context == :apply
        for e in rec
            @test e.start_ns >= top.start_ns
            @test e.start_ns + e.duration_ns <= top.start_ns + top.duration_ns + 1
        end
    end

    # closure caller: unstable compiler name -> labeled by source file
    top1 = only(filter(e -> e.label == :apply_total &&
                           e.transition_nr == tnrs[1], ev))
    @test startswith(string(top1.caller), "anon@")

    # all ranks must agree on the event counts (same topology, same model)
    local_n = length(ev)
    all_n = MPI.Allreduce(local_n, +, MPI.COMM_WORLD)
    @test all_n == Vahana.mpi.size * local_n

    # 3) named transition: exact caller label
    # NOTE: events carry the 0-based transition number of the apply! call;
    # sim.num_transitions is incremented AFTER the record is closed, so the
    # last record is addressed via maximum(transition_nr), not via
    # sim.num_transitions.
    apply!(sim, named_trans, A1, [A1, E1], [A1, E1])
    rec = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                 for e2 in
                                                 sim.instrumentation.events),
                 sim.instrumentation.events)
    top = only(filter(e -> e.label == :apply_total, rec))
    @test top.caller == :named_trans

    # 4) multiple agent types: per-type events, exact items (rank-local
    # slot counts; the sum over all ranks must be the global agent count)
    model2 = ModelTypes() |>
        register_agenttype!(A1) |>
        register_agenttype!(A2) |>
        register_edgetype!(E1) |>
        create_model("Instr MPI 2")
    sim2 = create_simulation(model2)
    a1ids = [ add_agent!(sim2, A1(i)) for i in 1:100 ]
    [ add_agent!(sim2, A2(i)) for i in 1:40 ]
    for i in 1:77
        add_edge!(sim2, a1ids[i], a1ids[i + 1], E1())
    end
    finish_init!(sim2)
    # E1 in write: only writable edgetypes get :edges_remove/:edges_add
    # events
    apply!(sim2, multi_trans, [A1, A2], [A1, A2], [A1, A2, E1])
    rec2 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim2.instrumentation.events),
                  sim2.instrumentation.events)

    trans2 = filter(e -> e.label == :transition, rec2)
    @test length(trans2) == 2
    @test Set(e.type for e in trans2) == Set((A1, A2))
    local_items = Dict{DataType, Int64}(A1 => 0, A2 => 0)
    for e in trans2
        local_items[e.type] += e.items
    end
    @test MPI.Allreduce(local_items[A1], +, MPI.COMM_WORLD) == 100
    @test MPI.Allreduce(local_items[A2], +, MPI.COMM_WORLD) == 40

    # edge transmission phases are recorded for every writable edgetype
    @test !isempty(filter(e -> e.label == :edges_remove && e.type === E1, rec2))
    @test !isempty(filter(e -> e.label == :edges_add && e.type === E1, rec2))

    # finish_write! sub-phases (Hook 12): A1/A2 are mortal (default), so
    # the died-join runs on every rank even without deaths
    @test length(filter(e -> e.label == :allreduce_mustcopy, rec2)) == 2
    @test length(filter(e -> e.label == :join_died, rec2)) == 2
    @test length(filter(e -> e.label == :win_shmdied, rec2)) == 2
    # join_died children: sizes + values, same type
    @test length(filter(e -> e.label == :join_sizes, rec2)) == 2
    @test length(filter(e -> e.label == :join_values, rec2)) == 2
    for e in filter(e -> e.label == :join_died, rec2)
        @test e.type in (A1, A2)
        @test e.kind == :phase
    end

    # all ranks must record the same (global) transition number
    tmin = minimum(e.transition_nr for e in rec2)
    tmax = maximum(e.transition_nr for e in rec2)
    @test tmin == tmax
    @test MPI.Allreduce(tmin, min, MPI.COMM_WORLD) ==
          MPI.Allreduce(tmax, max, MPI.COMM_WORLD)

    # 5) MPI collective events: kinds, count volumes, edge
    # payload bytes, alltoallv conservation, inverted state direction
    model_c = ModelTypes() |>
        register_agenttype!(A1) |>
        register_edgetype!(E1) |>
        create_model("Instr MPI comm")
    simc = create_simulation(model_c)
    [ add_agent!(simc, A1(i)) for i in 1:100 ]
    finish_init!(simc)
    # the transition adds one edge per agent to agent 1 of the next
    # rank: only edges with a remote target end up in the alltoall
    # buffer (local targets go directly into the edge container)
    tnr = Vahana.type_nr(simc, A1)
    # process_nr is 0-based (0 .. size-1); wrap around
    remotetarget = Vahana.agent_id(tnr, mod(Vahana.mpi.rank + 1,
                                            Vahana.mpi.size),
                                   Vahana.AgentNr(1))
    apply!(simc, A1, [A1, E1], [A1, E1]) do state, id, sim
        add_edge!(sim, id, remotetarget, E1())
        A1(state.x + 1)
    end
    recc = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  simc.instrumentation.events),
                  simc.instrumentation.events)

    # count exchange: exactly one Int64 per PE, volume must be present
    for l in (:agent_alltoall_ids, :edge_add_alltoall, :edge_remove_alltoall)
        for e in filter(x -> x.label == l, recc)
            @test e.kind == :alltoall
            @test e.comm !== nothing
            @test e.comm.send_items == Vahana.mpi.size
            @test e.comm.recv_items == Vahana.mpi.size
            @test e.comm.send_bytes == Vahana.mpi.size * sizeof(Int64)
            @test e.comm.recv_bytes == Vahana.mpi.size * sizeof(Int64)
        end
    end
    @test length(filter(e -> e.label == :edge_add_alltoall, recc)) == 1
    @test length(filter(e -> e.label == :edge_remove_alltoall, recc)) == 1

    # the agent state transfer runs only between nodes (multinode =
    # size > shmsize); on a single node it goes through the SHM window
    multinode = Vahana.mpi.size > Vahana.mpi.shmsize
    if multinode
        # E1 in read -> one ID exchange + one state exchange for A1
        @test length(filter(e -> e.label == :agent_alltoall_ids &&
                                 e.type === A1, recc)) == 1
        # ID transfer: bytes = items × sizeof(AgentID)
        for e in filter(x -> x.label == :agent_alltoallv_ids, recc)
            @test e.kind == :alltoallv
            @test e.comm !== nothing
            @test e.comm.send_bytes == e.comm.send_items * sizeof(Vahana.AgentID)
            @test e.comm.recv_bytes == e.comm.recv_items * sizeof(Vahana.AgentID)
        end
        # state transfer: the direction is inverted w.r.t. the ID transfer
        ids = only(filter(e -> e.label == :agent_alltoallv_ids, recc))
        st  = only(filter(e -> e.label == :agent_alltoallv_state, recc))
        @test st.comm.send_items == ids.comm.recv_items
        @test st.comm.recv_items == ids.comm.send_items
    else
        @test isempty(filter(e -> occursin("agent_alltoall",
                                           string(e.label)), recc))
    end

    # edge payload: storage type Tuple{AgentID, CE}, CE = Edge{E1} for
    # plain (non-stateless) edgetypes: bytes = items × sizeof
    for e in filter(x -> x.label == :edge_add_alltoallv, recc)
        @test e.kind == :alltoallv
        if e.comm !== nothing
            elemsize = sizeof(Vahana.AgentID) + sizeof(Vahana.Edge{E1})
            @test e.comm.send_bytes == e.comm.send_items * elemsize
            @test e.comm.recv_bytes == e.comm.recv_items * elemsize
        end
    end

    # alltoallv conservation: total send == total recv over all ranks
    # (mapreduce with init: `sum` over an empty generator throws)
    for l in (:agent_alltoallv_ids, :agent_alltoallv_state,
              :edge_add_alltoallv, :edge_remove_alltoallv)
        lsi = mapreduce(e -> e.comm === nothing ? 0 : e.comm.send_items, +,
                        init = 0, filter(x -> x.label == l, recc))
        lri = mapreduce(e -> e.comm === nothing ? 0 : e.comm.recv_items, +,
                        init = 0, filter(x -> x.label == l, recc))
        @test MPI.Allreduce(lsi, +, MPI.COMM_WORLD) ==
              MPI.Allreduce(lri, +, MPI.COMM_WORLD)
    end
    # the transition added edges -> the edge-add transfer has volume
    # somewhere
    vol = mapreduce(e -> e.comm === nothing ? 0 : e.comm.send_items, +,
                    init = 0,
                    filter(x -> x.label == :edge_add_alltoallv, recc))
    @test MPI.Allreduce(vol, +, MPI.COMM_WORLD) > 0

    # 6) diagnostic mode: per-agent timing statistics per agent type
    sim2.instrumentation.diagnostic = true
    apply!(sim2, multi_trans, [A1, A2], [A1, A2], [A1, A2, E1])
    sim2.instrumentation.diagnostic = false
    rec3 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim2.instrumentation.events),
                  sim2.instrumentation.events)
    # exactly one :transition_stats event per agent type in `call`, on
    # every rank — even for types with 0 local agents (the transition
    # loop runs empty, the event is still recorded)
    stats3 = filter(e -> e.label == :transition_stats, rec3)
    @test length(stats3) == 2
    stats3_bytype = Dict(e.type => e.agentstats for e in stats3)
    @test Set(keys(stats3_bytype)) == Set((A1, A2))
    # agentstats is nothing exactly for types with 0 local agents
    for (t, st) in stats3_bytype
        if st !== nothing
            @test st.min_ns <= st.max_ns
            @test st.sum_ns >= st.min_ns
            @test st.sum_sq_ns >= Int128(st.sum_ns)
        end
    end
    # at least one rank must have measured agent times (100 A1 agents are
    # distributed over all ranks)
    n_measured = sum(st !== nothing for st in values(stats3_bytype))
    @test MPI.Allreduce(n_measured, +, MPI.COMM_WORLD) > 0

    # diagnostic mode inserts a barrier before the payload collectives
    @test !isempty(filter(e -> e.label == :ready_wait_edge_add_alltoallv, rec3))
    for e in filter(x -> x.label in
                     (:ready_wait_edge_add_alltoallv,
                      :ready_wait_edge_remove_alltoallv), rec3)
        @test e.kind == :barrier
    end

    # 7) with_edge in a fresh simulation (edge read buffers must not be
    # consumed by a preceding non-with_edge apply! on the same simulation)
    model3 = ModelTypes() |>
        register_agenttype!(A1) |>
        register_edgetype!(E1) |>
        create_model("Instr MPI 3")
    sim3 = create_simulation(model3)
    aids = [ add_agent!(sim3, A1(i)) for i in 1:100 ]
    for i in 1:99
        add_edge!(sim3, aids[i], aids[i + 1], E1())
    end
    finish_init!(sim3)
    # with_edge transition: first argument is Val(A1), not the state
    apply!(sim3, A1, A1, A1; with_edge = E1) do state, id, sim
        A1(42)
    end
    rec4 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim3.instrumentation.events),
                  sim3.instrumentation.events)
    trans4 = filter(e -> e.label == :transition && e.type === A1, rec4)
    @test length(trans4) == 1
    @test MPI.Allreduce(trans4[1].items, +, MPI.COMM_WORLD) == 100
    top4 = only(filter(e -> e.label == :apply_total, rec4))
    @test top4.caller !== nothing
    @test top4.context == :apply

    # 8) spatial markers: rebuild with joins, then snhash cache
    # hit (the spatial type A1 is read-only; only A2 is written, so
    # finish_spatial_neighbors! never deletes A1's cached info)
    model4 = ModelTypes() |> register_agenttype!(A1) |> register_agenttype!(A2) |> register_edgetype!(E1) |> create_model("Instr MPI 4")
    sim4 = create_simulation(model4)
    [ add_agent!(sim4, A1(i)) for i in 1:100 ]
    [ add_agent!(sim4, A2(i)) for i in 1:10 ]
    finish_init!(sim4)
    sn4 = SpatialNeighbors(agenttypes = [A1], pos_field = :x)
    apply!(sim4, (s, id, sim) -> A2(s.x + 1), A2, [A1, A2], [A2];
           spatial_neighbors = sn4)
    rec5 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim4.instrumentation.events),
                  sim4.instrumentation.events)
    @test !isempty(filter(e -> e.label == :spatial_collect && e.type === A1,
                          rec5))
    @test !isempty(filter(e -> e.label == :spatial_kdtree && e.type === A1,
                          rec5))
    # the collect phase joins poss and the state values across all ranks;
    # purpose events carry no volume (plan §11), the comm stats are on the
    # `:join_values` children
    @test !isempty(filter(e -> e.label == :join_spatial_poss, rec5))
    @test !isempty(filter(e -> e.label == :join_spatial_edges, rec5))
    for e in filter(e -> e.label in (:join_spatial_poss, :join_spatial_edges),
                    rec5)
        @test e.comm === nothing
        @test e.kind == :phase
    end
    # A1-typed join_values: spatial poss (Int64) and states (A1), both 8 B
    av = filter(e -> e.label == :join_values && e.type === A1, rec5)
    @test length(av) == 2
    for e in av
        @test e.comm !== nothing
        @test e.comm.send_bytes == e.comm.send_items * sizeof(Int64)
    end
    apply!(sim4, (s, id, sim) -> A2(s.x + 1), A2, [A1, A2], [A2];
           spatial_neighbors = sn4)
    rec6 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim4.instrumentation.events),
                  sim4.instrumentation.events)
    @test !isempty(filter(e -> e.label == :spatial_cached && e.type === A1,
                          rec6))
    @test isempty(filter(e -> e.label == :spatial_collect, rec6))

    # 9) mapreduce: own top-level record with the
    # allreduce child (mpi.active here), correct global result
    n_tr = sim4.num_transitions
    global_sum = mapreduce(sim4, s -> s.x, +, A1)
    @test sim4.num_transitions == n_tr
    @test global_sum == sum(1:100)
    rec7 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                 for e2 in
                                                 sim4.instrumentation.events),
                  sim4.instrumentation.events)
    mrs = filter(e -> e.label == :mapreduce, rec7)
    @test length(mrs) == 1
    mtop = only(mrs)
    @test mtop.type === A1
    @test mtop.context == :mapreduce
    @test mtop.caller === nothing
    @test mtop.transition_nr == n_tr
    # the allreduce child carries the single reduction value
    mars = filter(e -> e.label == :mapreduce_allreduce, rec7)
    @test length(mars) == 1
    mar = only(mars)
    @test mar.kind == :allreduce
    @test mar.comm !== nothing
    @test mar.comm.send_items == 1 && mar.comm.recv_items == 1
    @test mar.comm.send_bytes == sizeof(Int64)
    @test sim4.instrumentation.current_context == :none

    # 10) collective toggle off: no new events on any rank
    enable_instrumentation!(false)
    n_before = length(sim.instrumentation.events)
    apply!(sim, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    @test length(sim.instrumentation.events) == n_before
    @test isempty(sim.instrumentation.stack)

    sleep(mpi.rank * 0.05)
end
