using Test
using Vahana
using MPI

enable_asserts(true)
suppress_warnings(true)

struct A1 x::Int64 end
struct A2 x::Int64 end
struct E1 end

# the `type` field of an InstEvent is a VString holding nameof(type)
@inline etype(e) = Symbol(String(e.type))
# comm volume is 0 for local events (no more `comm::Union{CommStats, Nothing}`)
@inline has_comm(e) = e.send_items > 0 || e.recv_items > 0

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
    # the model has no `step` global -> step is -1 in every event
    @test all(e -> e.step == -1, ev)
    @test length(filter(e -> e.label == Vahana.EL_apply_total, ev)) == 2
    @test length(filter(e -> e.label == Vahana.EL_barrier_pre_apply, ev)) == 2
    @test length(filter(e -> e.label == Vahana.EL_barrier_post_apply, ev)) == 2
    @test length(filter(e -> e.label == Vahana.EL_transition, ev)) == 2
    @test isempty(sim.instrumentation.stack)

    # per-rank invariants: one :apply_total per record, caller/context set,
    # consistent transition_nr, nesting inside the record interval
    tnrs = sort(collect(unique(e.transition_nr for e in ev)))
    @test length(tnrs) == 2
    for rec_tnr in tnrs
        rec = filter(e -> e.transition_nr == rec_tnr, ev)
        top = only(filter(e -> e.label == Vahana.EL_apply_total, rec))
        @test !isempty(String(top.caller))
        @test top.context == Vahana.EC_apply
        for e in rec
            @test e.start_ns >= top.start_ns
            @test e.start_ns + e.dur_ns <= top.start_ns + top.dur_ns + 1
        end
    end

    # closure caller: unstable compiler name -> labeled by source file
    top1 = only(filter(e -> e.label == Vahana.EL_apply_total &&
                           e.transition_nr == tnrs[1], ev))
    @test startswith(String(top1.caller), "anon@")

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
    top = only(filter(e -> e.label == Vahana.EL_apply_total, rec))
    @test String(top.caller) == "named_trans"

    # 4) multiple agent types: per-type events, exact rank-local live-agent
    # counts captured at apply! entry
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
    expected_items = Dict(nameof(A1) => Vahana.num_agents(sim2, A1, false),
                          nameof(A2) => Vahana.num_agents(sim2, A2, false))
    # E1 in write: only writable edgetypes get :edges_remove/:edges_add
    # events
    apply!(sim2, multi_trans, [A1, A2], [A1, A2], [A1, A2, E1])
    rec2 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim2.instrumentation.events),
                  sim2.instrumentation.events)

    trans2 = filter(e -> e.label == Vahana.EL_transition, rec2)
    @test length(trans2) == 2
    @test Set(etype(e) for e in trans2) == Set(nameof.((A1, A2)))
    local_items = Dict{Symbol, Int64}(nameof(A1) => 0, nameof(A2) => 0)
    for e in trans2
        @test e.items == expected_items[etype(e)]
        local_items[etype(e)] += e.items
    end
    @test MPI.Allreduce(local_items[nameof(A1)], +, MPI.COMM_WORLD) == 100
    @test MPI.Allreduce(local_items[nameof(A2)], +, MPI.COMM_WORLD) == 40

    # edge transmission phases are recorded for every writable edgetype
    @test !isempty(filter(e -> e.label == Vahana.EL_edges_remove && etype(e) == nameof(E1), rec2))
    @test !isempty(filter(e -> e.label == Vahana.EL_edges_add && etype(e) == nameof(E1), rec2))

    # finish_write! sub-phases (Hook 12): A1/A2 are mortal (default), so
    # the died-join runs on every rank even without deaths
    @test length(filter(e -> e.label == Vahana.EL_allreduce_mustcopy, rec2)) == 2
    @test length(filter(e -> e.label == Vahana.EL_join_died, rec2)) == 2
    @test length(filter(e -> e.label == Vahana.EL_win_shmdied, rec2)) == 2
    # join_died children: sizes + values, same type
    @test length(filter(e -> e.label == Vahana.EL_join_sizes, rec2)) == 2
    @test length(filter(e -> e.label == Vahana.EL_join_values, rec2)) == 2
    for e in filter(e -> e.label == Vahana.EL_join_died, rec2)
        @test etype(e) in (nameof(A1), nameof(A2))
        @test e.kind == Vahana.EK_phase
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
    for l in (Vahana.EL_agent_alltoall_ids, Vahana.EL_edge_add_alltoall,
              Vahana.EL_edge_remove_alltoall)
        for e in filter(x -> x.label == l, recc)
            @test e.kind == Vahana.EK_alltoall
            @test has_comm(e)
            @test e.send_items == Vahana.mpi.size
            @test e.recv_items == Vahana.mpi.size
            @test e.send_bytes == Vahana.mpi.size * sizeof(Int64)
            @test e.recv_bytes == Vahana.mpi.size * sizeof(Int64)
        end
    end
    @test length(filter(e -> e.label == Vahana.EL_edge_add_alltoall, recc)) == 1
    @test length(filter(e -> e.label == Vahana.EL_edge_remove_alltoall, recc)) == 1

    # the agent state transfer runs only between nodes (multinode =
    # size > shmsize); on a single node it goes through the SHM window
    multinode = Vahana.mpi.size > Vahana.mpi.shmsize
    if multinode
        # E1 in read -> one ID exchange + one state exchange for A1
        @test length(filter(e -> e.label == Vahana.EL_agent_alltoall_ids &&
                                 etype(e) == nameof(A1), recc)) == 1
        # ID transfer: bytes = items × sizeof(AgentID)
        for e in filter(x -> x.label == Vahana.EL_agent_alltoallv_ids, recc)
            @test e.kind == Vahana.EK_alltoallv
            @test has_comm(e)
            @test e.send_bytes == e.send_items * sizeof(Vahana.AgentID)
            @test e.recv_bytes == e.recv_items * sizeof(Vahana.AgentID)
        end
        # state transfer: the direction is inverted w.r.t. the ID transfer
        ids = only(filter(e -> e.label == Vahana.EL_agent_alltoallv_ids, recc))
        st  = only(filter(e -> e.label == Vahana.EL_agent_alltoallv_state, recc))
        @test st.send_items == ids.recv_items
        @test st.recv_items == ids.send_items
    else
        @test isempty(filter(e -> occursin("agent_alltoall",
                                           string(e.label)), recc))
    end

    # edge payload: storage type Tuple{AgentID, CE}, CE = Edge{E1} for
    # plain (non-stateless) edgetypes: bytes = items × sizeof
    for e in filter(x -> x.label == Vahana.EL_edge_add_alltoallv, recc)
        @test e.kind == Vahana.EK_alltoallv
        if has_comm(e)
            elemsize = sizeof(Vahana.AgentID) + sizeof(Vahana.Edge{E1})
            @test e.send_bytes == e.send_items * elemsize
            @test e.recv_bytes == e.recv_items * elemsize
        end
    end

    # alltoallv conservation: total send == total recv over all ranks
    for l in (Vahana.EL_agent_alltoallv_ids, Vahana.EL_agent_alltoallv_state,
              Vahana.EL_edge_add_alltoallv, Vahana.EL_edge_remove_alltoallv)
        lsi = mapreduce(e -> e.send_items, +,
                        init = 0, filter(x -> x.label == l, recc))
        lri = mapreduce(e -> e.recv_items, +,
                        init = 0, filter(x -> x.label == l, recc))
        @test MPI.Allreduce(lsi, +, MPI.COMM_WORLD) ==
              MPI.Allreduce(lri, +, MPI.COMM_WORLD)
    end
    # the transition added edges -> the edge-add transfer has volume
    # somewhere
    vol = mapreduce(e -> e.send_items, +, init = 0,
                    filter(x -> x.label == Vahana.EL_edge_add_alltoallv, recc))
    @test MPI.Allreduce(vol, +, MPI.COMM_WORLD) > 0

    # 6) diagnostic mode: per-agent timing statistics per agent type
    @test enable_diagnostics!(sim2, true)
    @test sim2.instrumentation.diagnostic
    @test !sim.instrumentation.diagnostic # setting is per simulation
    apply!(sim2, multi_trans, [A1, A2], [A1, A2], [A1, A2, E1])
    @test !enable_diagnostics!(sim2, false)
    @test !sim2.instrumentation.diagnostic
    rec3 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim2.instrumentation.events),
                  sim2.instrumentation.events)
    # exactly one :transition_stats event per agent type in `call`, on
    # every rank — even for types with 0 local agents (the transition
    # loop runs empty, the event is still recorded)
    stats3 = filter(e -> e.label == Vahana.EL_transition_stats, rec3)
    @test length(stats3) == 2
    stats3_bytype = Dict(etype(e) => e for e in stats3)
    @test Set(keys(stats3_bytype)) == Set(nameof.((A1, A2)))
    # flat agent stats are 0 exactly for types with 0 local agents
    for (t, e) in stats3_bytype
        if e.agent_sum_ns > 0
            @test e.agent_min_ns <= e.agent_max_ns
            @test e.agent_sum_ns >= e.agent_min_ns
            sum_sq = Vahana._inst_int128_reassemble(e.agent_sum_sq_hi,
                                                    e.agent_sum_sq_lo)
            @test sum_sq >= Int128(e.agent_sum_ns)
        end
    end
    # at least one rank must have measured agent times (100 A1 agents are
    # distributed over all ranks)
    n_measured = sum(e.agent_sum_ns > 0 for e in values(stats3_bytype))
    @test MPI.Allreduce(n_measured, +, MPI.COMM_WORLD) > 0

    # Agent counts are captured before apply!, so deaths during that apply!
    # do not change the count attached to its :transition event.
    live_a1 = Vahana.num_agents(sim2, A1, false)
    kill_tnr = sim2.num_transitions
    apply!(sim2, (state, id, sim) -> nothing, A1, [A1, E1], [A1, E1])
    kill_event = only(filter(e -> e.label == Vahana.EL_transition &&
                                  etype(e) == nameof(A1) &&
                                  e.transition_nr == kill_tnr,
                             sim2.instrumentation.events))
    @test kill_event.items == live_a1
    @test Vahana.num_agents(sim2, A1, false) == 0

    # State slots remain after deletion; the next event should report no live agents.
    empty_tnr = sim2.num_transitions
    apply!(sim2, (state, id, sim) -> state, A1, [A1, E1], [A1, E1])
    empty_event = only(filter(e -> e.label == Vahana.EL_transition &&
                                   etype(e) == nameof(A1) &&
                                   e.transition_nr == empty_tnr,
                              sim2.instrumentation.events))
    @test empty_event.items == 0

    # diagnostic mode inserts a barrier before the payload collectives
    @test !isempty(filter(e -> e.label == Vahana.EL_ready_wait_edge_add_alltoallv, rec3))
    for e in filter(x -> x.label in
                     (Vahana.EL_ready_wait_edge_add_alltoallv,
                      Vahana.EL_ready_wait_edge_remove_alltoallv), rec3)
        @test e.kind == Vahana.EK_barrier
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
    trans4 = filter(e -> e.label == Vahana.EL_transition && etype(e) == nameof(A1), rec4)
    @test length(trans4) == 1
    @test MPI.Allreduce(trans4[1].items, +, MPI.COMM_WORLD) == 100
    top4 = only(filter(e -> e.label == Vahana.EL_apply_total, rec4))
    @test !isempty(String(top4.caller))
    @test top4.context == Vahana.EC_apply

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
    @test !isempty(filter(e -> e.label == Vahana.EL_spatial_collect && etype(e) == nameof(A1),
                          rec5))
    @test !isempty(filter(e -> e.label == Vahana.EL_spatial_kdtree && etype(e) == nameof(A1),
                          rec5))
    # the collect phase joins poss and the state values across all ranks;
    # purpose events carry no volume (plan §11), the comm stats are on the
    # `:join_values` children
    @test !isempty(filter(e -> e.label == Vahana.EL_join_spatial_poss, rec5))
    @test !isempty(filter(e -> e.label == Vahana.EL_join_spatial_edges, rec5))
    for e in filter(e -> e.label in (Vahana.EL_join_spatial_poss,
                                     Vahana.EL_join_spatial_edges), rec5)
        @test !has_comm(e)
        @test e.kind == Vahana.EK_phase
    end
    # A1-typed join_values: spatial poss (Int64) and states (A1), both 8 B
    av = filter(e -> e.label == Vahana.EL_join_values && etype(e) == nameof(A1), rec5)
    @test length(av) == 2
    for e in av
        @test has_comm(e)
        @test e.send_bytes == e.send_items * sizeof(Int64)
    end
    apply!(sim4, (s, id, sim) -> A2(s.x + 1), A2, [A1, A2], [A2];
           spatial_neighbors = sn4)
    rec6 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                  for e2 in
                                                  sim4.instrumentation.events),
                  sim4.instrumentation.events)
    @test !isempty(filter(e -> e.label == Vahana.EL_spatial_cached && etype(e) == nameof(A1),
                          rec6))
    @test isempty(filter(e -> e.label == Vahana.EL_spatial_collect, rec6))

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
    mrs = filter(e -> e.label == Vahana.EL_mapreduce, rec7)
    @test length(mrs) == 1
    mtop = only(mrs)
    @test etype(mtop) == nameof(A1)
    @test mtop.context == Vahana.EC_mapreduce
    @test isempty(String(mtop.caller))
    @test mtop.transition_nr == n_tr
    # the allreduce child carries the single reduction value
    mars = filter(e -> e.label == Vahana.EL_mapreduce_allreduce, rec7)
    @test length(mars) == 1
    mar = only(mars)
    @test mar.kind == Vahana.EK_allreduce
    @test has_comm(mar)
    @test mar.send_items == 1 && mar.recv_items == 1
    @test mar.send_bytes == sizeof(Int64)
    @test sim4.instrumentation.current_context == :none

    # 10) collective toggle off: no new events on any rank
    enable_instrumentation!(false)
    n_before = length(sim.instrumentation.events)
    apply!(sim, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    @test length(sim.instrumentation.events) == n_before
    @test isempty(sim.instrumentation.stack)

    sleep(mpi.rank * 0.05)
end

# ---------------------------------------------------------------------------
# Init-phase hooks (distribute!/sendagents!/sendedges!)
# ---------------------------------------------------------------------------

@testset "instrumentation init phase" begin
    model2 = ModelTypes() |>
        register_agenttype!(A1) |>
        register_agenttype!(A2) |>
        register_edgetype!(E1; hints=[:Stateless]) |>
        create_model("Instr Init")

    enable_instrumentation!(true)

    # finish_init! with distribute calls distribute! internally
    sim_init = create_simulation(model2; filename="InstrumentationRecordMPI")
    a1_ids = [ add_agent!(sim_init, A1(i)) for i in 1:60 ]
    [ add_agent!(sim_init, A2(i)) for i in 1:40 ]
    for i in 1:length(a1_ids)
        add_edge!(sim_init, a1_ids[i], a1_ids[mod1(i+1, length(a1_ids))], E1())
    end
    finish_init!(sim_init)
    enable_instrumentation!(false)

    ev = sim_init.instrumentation.events
    @test !isempty(ev)

    # Filter to init-context events only (finish_init! also calls finish_write!
    # which emits :join_died / :allreduce_* events with context=:none)
    init_ev = filter(e -> e.context == Vahana.EC_init, ev)
    @test !isempty(init_ev)

    # top-level :distribute event
    dist = filter(e -> e.label == Vahana.EL_distribute, ev)
    @test length(dist) == 1
    @test dist[1].context == Vahana.EC_init
    @test isempty(String(dist[1].caller))
    @test etype(dist[1]) == nameof(Nothing)

    # per-type :sendagents events (one per node type)
    sa = filter(e -> e.label == Vahana.EL_sendagents, init_ev)
    @test length(sa) == 2
    @test length(unique(etype(e) for e in sa)) == 2

    # :agent_init_alltoall / :agent_init_alltoallv per type
    aia = filter(e -> e.label == Vahana.EL_agent_init_alltoall, init_ev)
    aiaa = filter(e -> e.label == Vahana.EL_agent_init_alltoallv, init_ev)
    @test length(aia) == 2
    @test length(aiaa) == 2
    @test all(e -> e.kind == Vahana.EK_alltoall, aia)
    @test all(e -> e.kind == Vahana.EK_alltoallv, aiaa)
    @test all(has_comm, aiaa)

    # per-type :sendedges events
    se = filter(e -> e.label == Vahana.EL_sendedges, init_ev)
    @test length(se) == 1
    @test etype(se[1]) == nameof(E1)

    # :edge_add_alltoall / :edge_add_alltoallv from edges_alltoall! (init context)
    eaa = filter(e -> e.label == Vahana.EL_edge_add_alltoall, init_ev)
    eaav = filter(e -> e.label == Vahana.EL_edge_add_alltoallv, init_ev)
    @test length(eaa) == 1
    @test length(eaav) == 1

    # join purpose events for idmapping
    jk = filter(e -> e.label == Vahana.EL_join_idmapping_keys, init_ev)
    jv = filter(e -> e.label == Vahana.EL_join_idmapping_values, init_ev)
    @test length(jk) == 1
    @test length(jv) == 1
    # mechanism events from the idmapping joins (context :init)
    @test length(filter(e -> e.label == Vahana.EL_join_sizes && e.context == Vahana.EC_init, ev)) == 2
    @test length(filter(e -> e.label == Vahana.EL_join_values && e.context == Vahana.EC_init, ev)) == 2

    # barrier events
    @test length(filter(e -> e.label == Vahana.EL_barrier_pre_distribute, init_ev)) == 1
    @test length(filter(e -> e.label == Vahana.EL_barrier_post_distribute, init_ev)) == 1

    # raster_bcast
    @test length(filter(e -> e.label == Vahana.EL_raster_bcast, init_ev)) == 1

    # nesting: all init-context events inside :distribute
    top = only(dist)
    for e in init_ev
        @test e.start_ns >= top.start_ns &&
              e.start_ns + e.dur_ns <= top.start_ns + top.dur_ns + 1
    end

    # context reset after distribute!
    @test sim_init.instrumentation.current_context == :none
    @test sim_init.instrumentation.current_caller === nothing
    @test isempty(sim_init.instrumentation.stack)

    # Parallel HDF5 compound-event save/load roundtrip.
    expected_events = MPI.Allreduce(length(ev), +, mpi.comm)
    write_instrumentation!(sim_init)
    close_h5file!(sim_init)
    loaded = load_instrumentation("InstrumentationRecordMPI")
    @test eltype(loaded) == InstEvent
    @test length(loaded) == expected_events
    # every rank's events must carry that rank
    @test Set(Int(e.rank) for e in loaded) == Set(0:(mpi.size - 1))
    @test any(e -> e.label == Vahana.EL_distribute, loaded)
    @test any(e -> isempty(String(e.caller)), loaded)
    @test any(e -> etype(e) == nameof(Nothing), loaded)
    # no `step` global -> the -1 sentinel survives the roundtrip
    @test all(e -> e.step == -1, loaded)
    # sorted by start time, same order as the save
    @test isempty(loaded) || all(i ->
              (loaded[i].start_ns, -loaded[i].dur_ns) <=
              (loaded[i + 1].start_ns, -loaded[i + 1].dur_ns),
              1:length(loaded) - 1)
    if mpi.rank == 0
        for rank in 0:(mpi.size - 1)
            rm("h5/InstrumentationRecordMPI_$(rank).h5"; force=true)
        end
        rm("h5/InstrumentationRecordMPI.h5"; force=true)
    end
    MPI.Barrier(mpi.comm)

    sleep(mpi.rank * 0.05)
end

# ---------------------------------------------------------------------------
# step global: events carry the value of the simulation global `step` if
# the globals struct has such a field, and the -1 sentinel otherwise
# ---------------------------------------------------------------------------

@testset "instrumentation step global" begin
    mutable struct StepGlobals
        step::Int64
        other::String
    end

    mutable struct NoStepGlobals
        other::String
    end

    model_s = ModelTypes() |>
        register_agenttype!(A1) |>
        register_edgetype!(E1) |>
        create_model("Instr Step")

    enable_instrumentation!(true)

    # globals WITH a `step` field: every event carries the value at the
    # time the event is recorded
    sim_s = create_simulation(model_s, nothing, StepGlobals(0, "x"),
                              filename = "InstrumentationStepMPI")
    [ add_agent!(sim_s, A1(i)) for i in 1:100 ]
    finish_init!(sim_s)
    apply!(sim_s, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    rec1 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                 for e2 in
                                                 sim_s.instrumentation.events),
                  sim_s.instrumentation.events)
    @test !isempty(rec1)
    @test any(e -> e.label == Vahana.EL_apply_total, rec1)
    @test all(e -> e.step == 0, rec1)

    # set_global! before the next apply! -> all events of the new record
    # carry 42, the earlier record keeps its value
    set_global!(sim_s, :step, 42)
    apply!(sim_s, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    rec2 = filter(e -> e.transition_nr == maximum(e2.transition_nr
                                                 for e2 in
                                                 sim_s.instrumentation.events),
                  sim_s.instrumentation.events)
    @test !isempty(rec2)
    @test any(e -> e.label == Vahana.EL_apply_total, rec2)
    @test all(e -> e.step == 42, rec2)
    @test all(e -> e.step == 0, rec1)

    # globals WITHOUT a `step` field: step stays -1 in every event
    sim_ns = create_simulation(model_s, nothing, NoStepGlobals("y"))
    [ add_agent!(sim_ns, A1(i)) for i in 1:50 ]
    finish_init!(sim_ns)
    apply!(sim_ns, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    ev_ns = sim_ns.instrumentation.events
    @test !isempty(ev_ns)
    @test all(e -> e.step == -1, ev_ns)

    # save/load roundtrip preserves the per-record step values
    write_instrumentation!(sim_s)
    close_h5file!(sim_s)
    # the loader returns all ranks' events combined
    n_loaded = MPI.Allreduce(length(sim_s.instrumentation.events), +, mpi.comm)
    loaded = load_instrumentation("InstrumentationStepMPI")
    @test length(loaded) == n_loaded
    @test Set(Int(e.step) for e in loaded) == Set((0, 42))
    @test any(e -> e.step == 42 && e.label == Vahana.EL_apply_total, loaded)

    enable_instrumentation!(false)

    if mpi.rank == 0
        for rank in 0:(mpi.size - 1)
            rm("h5/InstrumentationStepMPI_$(rank).h5"; force=true)
        end
        rm("h5/InstrumentationStepMPI.h5"; force=true)
    end
    MPI.Barrier(mpi.comm)

    sleep(mpi.rank * 0.05)
end
