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

    # all ranks must record the same (global) transition number
    tmin = minimum(e.transition_nr for e in rec2)
    tmax = maximum(e.transition_nr for e in rec2)
    @test tmin == tmax
    @test MPI.Allreduce(tmin, min, MPI.COMM_WORLD) ==
          MPI.Allreduce(tmax, max, MPI.COMM_WORLD)

    # 5) diagnostic mode: per-agent timing statistics per agent type
    sim2.instrumentation.diagnostic = true
    apply!(sim2, multi_trans, [A1, A2], [A1, A2], [A1, A2])
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

    # 6) with_edge in a fresh simulation (edge read buffers must not be
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

    # 7) collective toggle off: no new events on any rank
    enable_instrumentation!(false)
    n_before = length(sim.instrumentation.events)
    apply!(sim, (s, id, sim) -> A1(s.x + 1), A1, [A1, E1], [A1, E1])
    @test length(sim.instrumentation.events) == n_before
    @test isempty(sim.instrumentation.stack)

    sleep(mpi.rank * 0.05)
end
