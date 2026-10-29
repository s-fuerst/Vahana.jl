export Instrumentation, InstEvent
export enable_instrumentation!, enable_diagnostics!
export write_instrumentation!, load_instrumentation

@enum EventContext begin
    EC_none
    EC_apply
    EC_mapreduce
    EC_init
end

@enum EventKind begin
    EK_phase      # Default in _inst_begin!/_inst_end!
    EK_barrier
    EK_alltoall
    EK_alltoallv
    EK_allreduce
    EK_win_fence
end

@enum EventLabel begin
    # --- MPI.jl: Init & Distribution ---
    EL_distribute
    EL_barrier_pre_distribute
    EL_barrier_post_distribute
    EL_raster_bcast

    EL_agent_init_alltoall
    EL_agent_init_alltoallv
    EL_ready_wait_agent_init_alltoallv
    EL_agent_alltoall_ids
    EL_agent_alltoallv_ids
    EL_ready_wait_agent_alltoallv_ids
    EL_agent_alltoall_state
    EL_agent_alltoallv_state
    EL_ready_wait_agent_alltoallv_state
    EL_edge_add_alltoall
    EL_edge_add_alltoallv
    EL_ready_wait_edge_add_alltoallv
    EL_edge_remove_alltoall
    EL_edge_remove_alltoallv
    EL_ready_wait_edge_remove_alltoallv

    # --- Simulation.jl: apply! & Spatial ---
    EL_apply_total
    EL_prepare_read
    EL_prepare_write
    EL_barrier_pre_apply
    EL_transition
    EL_barrier_post_apply
    EL_edges_add
    EL_edges_remove
    EL_finish_read
    EL_finish_write
    EL_finish_spatial
    EL_memcpy_state
    EL_win_shmstate
    EL_win_shmdied
    EL_spatial_collect
    EL_spatial_kdtree
    EL_spatial_cached
    EL_allreduce_mustcopy
    EL_allreduce_networkchanged
    EL_sendagents
    EL_sendedges

    # --- AgentMethods.jl: mapreduce / join / Diagnostics ---
    EL_mapreduce
    EL_mapreduce_local
    EL_mapreduce_allreduce
    EL_join_died
    EL_join_sizes
    EL_ready_wait_join_sizes
    EL_join_values
    EL_ready_wait_join_values
    EL_join_idmapping_keys
    EL_join_idmapping_values
    EL_join_spatial_poss
    EL_join_spatial_edges
    EL_transition_stats   # nur diagnostic

    # --- Test-only (test/instrumentation_smoke.jl) ---
    # EL_bench
    # EL_probe
end

# ---------------------------------------------------------------------------
# Symbol -> enum conversion
#
# The call sites (MPI.jl, Simulation.jl, AgentMethods.jl) still pass plain
# Symbols. These dicts translate them at the _inst_begin!/_inst_end! boundary
# into the enum values stored in the InstEvent.
# ---------------------------------------------------------------------------

const _EVENT_CONTEXT_BY_SYMBOL = Dict{Symbol, EventContext}(
    :none       => EC_none,
    :apply      => EC_apply,
    :mapreduce  => EC_mapreduce,
    :init       => EC_init,
)

const _EVENT_KIND_BY_SYMBOL = Dict{Symbol, EventKind}(
    :phase      => EK_phase,
    :barrier    => EK_barrier,
    :alltoall   => EK_alltoall,
    :alltoallv  => EK_alltoallv,
    :allreduce  => EK_allreduce,
    :win_fence  => EK_win_fence,
)

const _EVENT_LABEL_BY_SYMBOL = Dict{Symbol, EventLabel}(
    :distribute                         => EL_distribute,
    :barrier_pre_distribute             => EL_barrier_pre_distribute,
    :barrier_post_distribute            => EL_barrier_post_distribute,
    :raster_bcast                       => EL_raster_bcast,
    :agent_init_alltoall                => EL_agent_init_alltoall,
    :agent_init_alltoallv               => EL_agent_init_alltoallv,
    :ready_wait_agent_init_alltoallv    => EL_ready_wait_agent_init_alltoallv,
    :agent_alltoall_ids                 => EL_agent_alltoall_ids,
    :agent_alltoallv_ids                => EL_agent_alltoallv_ids,
    :ready_wait_agent_alltoallv_ids     => EL_ready_wait_agent_alltoallv_ids,
    :agent_alltoall_state               => EL_agent_alltoall_state,
    :agent_alltoallv_state              => EL_agent_alltoallv_state,
    :ready_wait_agent_alltoallv_state   => EL_ready_wait_agent_alltoallv_state,
    :edge_add_alltoall                  => EL_edge_add_alltoall,
    :edge_add_alltoallv                 => EL_edge_add_alltoallv,
    :ready_wait_edge_add_alltoallv      => EL_ready_wait_edge_add_alltoallv,
    :edge_remove_alltoall               => EL_edge_remove_alltoall,
    :edge_remove_alltoallv              => EL_edge_remove_alltoallv,
    :ready_wait_edge_remove_alltoallv   => EL_ready_wait_edge_remove_alltoallv,
    :apply_total                        => EL_apply_total,
    :prepare_read                       => EL_prepare_read,
    :prepare_write                      => EL_prepare_write,
    :barrier_pre_apply                  => EL_barrier_pre_apply,
    :transition                         => EL_transition,
    :barrier_post_apply                 => EL_barrier_post_apply,
    :edges_add                          => EL_edges_add,
    :edges_remove                       => EL_edges_remove,
    :finish_read                        => EL_finish_read,
    :finish_write                       => EL_finish_write,
    :finish_spatial                     => EL_finish_spatial,
    :memcpy_state                       => EL_memcpy_state,
    :win_shmstate                       => EL_win_shmstate,
    :win_shmdied                        => EL_win_shmdied,
    :spatial_collect                    => EL_spatial_collect,
    :spatial_kdtree                     => EL_spatial_kdtree,
    :spatial_cached                     => EL_spatial_cached,
    :allreduce_mustcopy                 => EL_allreduce_mustcopy,
    :allreduce_networkchanged           => EL_allreduce_networkchanged,
    :sendagents                         => EL_sendagents,
    :sendedges                          => EL_sendedges,
    :mapreduce                          => EL_mapreduce,
    :mapreduce_local                    => EL_mapreduce_local,
    :mapreduce_allreduce                => EL_mapreduce_allreduce,
    :join_died                          => EL_join_died,
    :join_sizes                         => EL_join_sizes,
    :ready_wait_join_sizes              => EL_ready_wait_join_sizes,
    :join_values                        => EL_join_values,
    :ready_wait_join_values             => EL_ready_wait_join_values,
    :join_idmapping_keys                => EL_join_idmapping_keys,
    :join_idmapping_values              => EL_join_idmapping_values,
    :join_spatial_poss                  => EL_join_spatial_poss,
    :join_spatial_edges                 => EL_join_spatial_edges,
    :transition_stats                   => EL_transition_stats,
)

# Per-agent timing accumulator used only when diagnostic instrumentation is
# enabled. It is deliberately not part of the persisted event representation.
mutable struct AgentTimeStats
    n::Int64
    min_ns::Int64
    max_ns::Int64
    sum_ns::Int64
    sum_sq_ns::Int128

    AgentTimeStats() = new(0, typemax(Int64), typemin(Int64), 0, 0)
end

@inline function _inst_add_agent_time!(stats::AgentTimeStats, dt::Integer)
    dt_ns = Int64(dt)
    stats.n += 1
    stats.min_ns = min(stats.min_ns, dt_ns)
    stats.max_ns = max(stats.max_ns, dt_ns)
    stats.sum_ns += dt_ns
    stats.sum_sq_ns += Int128(dt_ns) * Int128(dt_ns)
    stats
end

@inline _inst_agentstats_or_nothing(stats::AgentTimeStats) =
    stats.n == 0 ? nothing : stats

# The fixed-width string fields (caller, type) use the `VString{N}`
# conversion from HDF5.jl: `create_string_converter` (called by
# `enable_instrumentation!`) generates the `SVector{N, UInt8}(str)`
# constructor that pads with zeros (and truncates strings that are too
# long). The conversion is only used at event construction time in
# `_inst_end!`, which runs only when instrumentation is enabled.

# Fixed-width, HDF5-compound representation of one event. Symbol and
# DataType values are represented by dictionary IDs; Int128 is stored Hi/Lo.
struct InstEvent
    label::EventLabel
    # transition name, nothing outside of apply!
    caller::VString{32}
    type::VString{32}
    # event origin: :apply, :mapreduce, :init, (later :load_balancing),
    # :none (orthogonal to kind)
    context::EventContext
    # :phase / :barrier / :alltoall / :alltoallv / :allreduce / :win_fence
    kind::EventKind
    # MPI rank that recorded the event
    rank::Int32
    # step is only set in the case that the simulation has a global
    # variable named step
    step::Int64
    transition_nr::Int64
    # Event-local item count. For :transition this is live agents at apply! entry;
    # for :transition_stats it is the number of timed agent callbacks.
    items::Int64
    diag::Bool
    start_ns::Int64
    dur_ns::Int64
    # send/recv volume; 0 for local event
    send_items::Int64
    recv_items::Int64
    send_bytes::Int64
    recv_bytes::Int64
    # diagnostic-only 0 otherwive
    agent_min_ns::Int64
    agent_max_ns::Int64
    agent_sum_ns::Int64
    agent_sum_sq_hi::Int64
    agent_sum_sq_lo::Int64
end




# A measured interval. parent durations include child durations.
# struct InstEvent
#     label::Symbol
#     # transition name, nothing outside of apply!
#     caller::Union{Nothing, Symbol}
#     # event origin: :apply, :mapreduce, :init, :load_balancing,
#     # :none (orthogonal to kind)
#     context::Symbol
#     # agent or edge type, Nothing = not type-specific
#     type::DataType
#     transition_nr::Int64
#     start_ns::Int64
#     duration_ns::Int64
#     # local item count (agents, edges, ...)
#     items::Int64
#     # send/recv volume; nothing for local events
#     comm::Union{CommStats, Nothing}
#     # per-agent timing statistics; nothing except for :transition_stats
#     # (diagnostic-only)
#     agentstats::Union{AgentTimeStats, Nothing}
#     # :phase / :barrier / :alltoall / :alltoallv / :allreduce / :win_fence
#     kind::Symbol

#     function InstEvent(label, caller, context, type, transition_nr,
#                 start_ns, duration_ns, kind;
#                 items = 0, comm = nothing, agentstats = nothing)
#         new(label, caller, context, type, transition_nr, start_ns, duration_ns,
#             items, comm, agentstats, kind)
#     end
# end

mutable struct Instrumentation
    # true: additional barriers before collectives
    # (distorts the timings!)
    diagnostic::Bool                 
    events::Vector{InstEvent}
    # copied into the events at _inst_end!; after apply! -> nothing
    current_caller::Union{Nothing, Symbol}   
    # event context (default :none)
    current_context::Symbol
    # func -> name cache
     func_labels::Dict{Any, Symbol}
    # (label, type, t0); LIFO nesting
    stack::Vector{Tuple{Symbol, DataType, Int64}} 

    function Instrumentation()
        new(false, InstEvent[], nothing, :none, 
            Dict{Any, Symbol}(), Tuple{Symbol, DataType, Int64}[])
    end
end

# making this a function results in code being invalidated and recompiled when
# this gets changed
instrumentation_enabled() = false

"""
    enable_instrumentation!(enable::Bool)

Module-level switch (analogous to [`enable_asserts`](@ref)) that turns all
instrumentation hooks into no-ops, without touching individual simulations.
Like `enable_asserts`, changes take effect only after a world-age barrier
(top level or in subsequently called functions).

When `enable` is set to true, this function also calls [`create_enum_converter`](@ref)
and [`create_string_converter`](@ref), as this functionality is utilized to
store the instrumentation to the HDF5 file.
"""
function enable_instrumentation!(enable::Bool)
    if enable
        create_enum_converter()
        create_string_converter(false)
        @eval instrumentation_enabled() = true
    else
        @eval instrumentation_enabled() = false
    end
end

"""
    enable_diagnostics!(sim::Simulation, enable::Bool)

Enable or disable diagnostic instrumentation for one simulation.

When enabled, Vahana records per-agent transition timings in
`:transition_stats` events and inserts additional barriers before selected
MPI collectives to expose rank imbalance and waiting time. These measurements
add overhead and perturb timings; use them for diagnosis, not performance
benchmarks.

This setting is independent of [`enable_instrumentation!`](@ref): diagnostics
are only recorded when instrumentation is also enabled. In MPI runs, call
this function on every rank with the same `enable` value before the measured
operation, since ranks with different settings may take different collective
paths.

Returns the value of `enable`.
"""
function enable_diagnostics!(sim::Simulation, enable::Bool)
    sim.instrumentation.diagnostic = enable
end

# Stack: begin = push(label, type, time_ns), end = pop -> duration.
# LIFO covers nested phases. label/type come from the stack entry; the
# parameters at end are used for consistency checks only.
@inline function _inst_begin!(sim, label, type::DataType = Nothing)
    if instrumentation_enabled()
        push!(sim.instrumentation.stack, (label, type, time_ns()))
    end
    nothing
end

@inline function _inst_end!(sim, label, type::DataType = Nothing;
                  kind::Symbol = :phase,
                  si::Integer = 0, ri::Integer = 0,
                  sb::Integer = 0, rb::Integer = 0,
                  items::Integer = 0,
                  agentstats::Union{Nothing, AgentTimeStats} = nothing)
    if instrumentation_enabled()
        inst = sim.instrumentation
        (l, t, t0) = pop!(inst.stack)
        if l !== label
            error("event stack unbalanced: expected label $l, got $label")
        end
        if t !== type
            error("event stack unbalanced: expected type $t, got $type")
        end
        now = time_ns()
        # convert the Symbol API to the fixed-width event representation
        label_id = get(_EVENT_LABEL_BY_SYMBOL, label, nothing)
        label_id === nothing &&
            error("unknown instrumentation event label: $label")
        caller = inst.current_caller === nothing ?
            VString{32}("") : VString{32}(string(inst.current_caller))
        context_id = get(_EVENT_CONTEXT_BY_SYMBOL, inst.current_context, nothing)
        context_id === nothing &&
            error("unknown instrumentation event context: $(inst.current_context)")
        kind_id = get(_EVENT_KIND_BY_SYMBOL, kind, nothing)
        kind_id === nothing &&
            error("unknown instrumentation event kind: $kind")

        step = if hasfield(typeof(sim.globals), :step)
            get_global(sim, :step)
        else
            -1
        end
        
        typ = VString{32}(string(nameof(t)))
        agent_min_ns, agent_max_ns, agent_sum_ns = if agentstats === nothing || agentstats.n == 0
            (Int64(0), Int64(0), Int64(0))
        else
            (agentstats.min_ns, agentstats.max_ns, agentstats.sum_ns)
        end
        agent_sum_sq_hi, agent_sum_sq_lo = agentstats === nothing || agentstats.n == 0 ?
            (Int64(0), Int64(0)) : _inst_int128_split(agentstats.sum_sq_ns)
        push!(inst.events, InstEvent(label_id,
                                     caller,
                                     typ,
                                     context_id,
                                     kind_id,
                                     Int32(mpi.rank),
                                     step,
                                     sim.num_transitions,
                                     items,
                                     startswith(string(label), "ready_wait_"),
                                     t0,
                                     now - t0,
                                     si, ri, sb, rb,
                                     agent_min_ns, agent_max_ns, agent_sum_ns,
                                     agent_sum_sq_hi, agent_sum_sq_lo))
    end
    nothing
end

# Snapshot live local agents at a stable read-state boundary. `reuseable`
# contains the dead slots and is kept in sync at finish_write!, so this is O(1)
# for mortal types. Immortal types have no reusable slots; `nextid` is the
# canonical count used by num_agents. Call only on the enabled instrumentation
# path, before the transition loop starts.
@inline function _inst_live_agent_count(sim, T::DataType)
    field = simfield(sim, T)
    if :Immortal in sim.typeinfos.nodes_attr[T][:hints]
        Int64(field.nextid - 1)
    else
        Int64(length(field.read.state) - length(field.read.reuseable))
    end
end

# Diagnostic-only per-agent timing. The event is still emitted for a type
# with no local agents (items == 0, timing fields == 0).
@inline function _inst_begin_agent_stats(sim, type::DataType)
    if instrumentation_enabled() && sim.instrumentation.diagnostic
        _inst_begin!(sim, :transition_stats, type)
        AgentTimeStats()
    else
        nothing
    end
end

@inline function _inst_end_agent_stats(sim, type::DataType,
                                      stats::Union{Nothing, AgentTimeStats})
    if stats !== nothing
        _inst_end!(sim, :transition_stats, type;
                   items = stats.n,
                   agentstats = _inst_agentstats_or_nothing(stats))
    end
    nothing
end

@inline function _inst_timed_transition_call(stats::AgentTimeStats, tfunc,
                                             a, b, c)
    t0 = time_ns()
    result = tfunc(a, b, c)
    _inst_add_agent_time!(stats, time_ns() - t0)
    result
end

# Wraps a phase in one `label` event (replaces the manual
# `_inst_begin!`/…/`_inst_end!` pairs). `label`/`type` are given exactly once,
# so begin and end cannot drift apart. Body as do-block:
#
#     _inst_phase(sim, :barrier_pre_apply; kind = :barrier) do
#         MPI.Barrier(MPI.COMM_WORLD)
#     end
#
# NOTE: `continue`, `break`, `return` or a throw inside `f()` skips the
# `_inst_end!` (unbalanced stack) — the wrapped call sites in `apply!` are
# plain statements. `items` is evaluated at the call site (before `f()`); for
# the slot counts used in `apply!` the length does not change inside `f()`.
@inline function _inst_phase(f::F, sim, label::Symbol,
                             type::DataType = Nothing;
                             kind::Symbol = :phase,
                             items::Integer = 0,
                             si::Integer = 0, ri::Integer = 0,
                             sb::Integer = 0, rb::Integer = 0) where F
    _inst_begin!(sim, label, type)
    result = f()
    _inst_end!(sim, label, type; kind = kind, items = items,
              si = si, ri = ri, sb = sb, rb = rb)
    result
end

# Uninstrumented pass-through for call sites that may run without a
# simulation (e.g. the user-level `join` without the `sim` kwarg).
# Dispatches only when sim is literally `nothing`.
@inline function _inst_phase(f::F, ::Nothing, label::Symbol,
                             type::DataType = Nothing;
                             kind::Symbol = :phase,
                             items::Integer = 0,
                             si::Integer = 0, ri::Integer = 0,
                             sb::Integer = 0, rb::Integer = 0) where F
    f()
end

# Caller labeling: named functions -> Symbol(string(func)).
# Closures (compiler names, starting with "#": "#674#675{...}" <= 1.11,
# "#2" >= 1.12) are unstable per compile session -> use the definition
# location instead:
#   Julia <= 1.11: Base.source_location(func) -> :anon_<file>:<line>
#   Julia >= 1.12: source_location is gone; only the file is available via
#                  Core.Compiler.code_lowered(...).debuginfo (line = 0)
#                  -> :anon_<file>
# Fallback :anonymous when no source can be determined (e.g.
# "unknown file name", GeneratedFunctionStubs).
function _inst_caller_label(sim, func)
    get!(sim.instrumentation.func_labels, func) do
        name = string(func)
        if !startswith(name, '#')
            return Symbol(name)
        end
        try
            if isdefined(Base, :source_location)
                line, file = Base.source_location(func)
                file == "unknown file name" ? :anonymous :
                    Symbol("anon@$(splitdir(file)[end]):$line")
            else
                di = Core.Compiler.code_lowered(func)[1].debuginfo
                file, line = Base.IRShow.debuginfo_firstline(di)
                filestr = string(file)
                isempty(filestr) || filestr == "unknown file name" ? :anonymous :
                    (line > 0 ? Symbol("anon@$(splitdir(filestr)[end]):$line") :
                    Symbol("anon@$(splitdir(filestr)[end])"))
            end
        catch
            :anonymous
        end
    end
end

# set current_caller/current_context in one place. The
# `func = :none` sentinel doubles as the reset (e.g. `_inst_set_context!(sim,
# :none)` after a top-level block)
@inline function _inst_set_context!(sim, context, func = :none)
    if instrumentation_enabled()
        inst = sim.instrumentation
        inst.current_caller = func === :none ? nothing :
            _inst_caller_label(sim, func)
        inst.current_context = context
    end
    sim
end

# Diagnostic mode: barrier inserted before collectives with payload.
# High ready_wait -> imbalance of the preceding phase; high collective
# duration with small bytes -> latency/network. DISTORTS TIMINGS -> for
# diagnosis only, not for benchmarking.
@inline function _inst_diag_barrier(sim, label)
    if instrumentation_enabled()
        if sim.instrumentation.diagnostic 
            _inst_begin!(sim, label)
            MPI.Barrier(MPI.COMM_WORLD)
            _inst_end!(sim, label; kind = :barrier)
        end
    end
    nothing
end

# Uninstrumented pass-through, cf. `_inst_phase`.
@inline function _inst_diag_barrier(::Nothing, label)
    nothing
end

# ---------------------------------------------------------------------------
# HDF5 save/load for the instrumentation event stream
# ---------------------------------------------------------------------------

# Int128 split/reassembly (HDF5.jl does not support Int128 natively)

const _INST_MASK64 = Int128(0xFFFFFFFFFFFFFFFF)
const _INST_TWO64  = Int128(0x10000000000000000)  # 2^64
const _INST_SIGN64 = Int128(0x8000000000000000)  # 2^63

"""Split an `Int128` into (hi, lo) Int64 parts using two's complement.
`hi` is the sign-extended upper 64 bits, `lo` is the lower 64 bits
(reinterpreted as signed)."""
function _inst_int128_split(value::Int128)
    lo_masked = value & _INST_MASK64
    lo = if lo_masked < _INST_SIGN64
        Int64(lo_masked)
    else
        Int64(lo_masked - _INST_TWO64)
    end
    hi = Int64(value >> 64)
    (hi, lo)
end

"""Reassemble an `Int128` from its (hi, lo) Int64 parts."""
function _inst_int128_reassemble(hi::Int64, lo::Int64)
    unsigned_lo = if lo >= 0
        Int128(lo)
    else
        Int128(lo) + _INST_TWO64
    end
    Int128(hi) * _INST_TWO64 + unsigned_lo
end

# ---------------------------------------------------------------------------
# write_instrumentation!
# ---------------------------------------------------------------------------

"""
    write_instrumentation!(sim)

Saves the current instrumentation event stream to the attached HDF5 file
(`sim.h5file`), under the group `/instrumentation/`.

This function is **collective**: all ranks must call it together. It uses
the same parallel/non-parallel HDF5 strategy as `write_agents`/`write_edges`:
with parallel HDF5 all ranks write partitioned datasets into one shared file;
without parallel HDF5 each rank writes into its own per-rank file.

Events are stored as one HDF5 compound dataset at `/instrumentation/events`
using the `InstEvent` struct directly: the enum fields (`label`, `context`,
`kind`) are stored as their integer representation, `caller` and `type` as
fixed-width byte strings (`VString{32}`). `rank` holds the MPI rank that
recorded the event. `agent_sum_sq` (Int128) is split into
`agent_sum_sq_hi`/`agent_sum_sq_lo` (each Int64).

The event stream is **not** cleared after saving. A second call to
`write_instrumentation!` on the same file will error because the
`/instrumentation/` group already exists.

See also [`load_instrumentation`](@ref).
"""
function write_instrumentation!(sim::Simulation)
    if sim.h5file === nothing
        create_h5file!(sim)
    end

    fid = sim.h5file

    # Check that /instrumentation/ does not already exist
    if haskey(fid, "instrumentation")
        error("The group /instrumentation/ already exists in this file.")
    end

    events = sim.instrumentation.events
    n_local = length(events)

    # --- prepare local data: sort ---
    sort!(events, by = e -> (e.start_ns, -e.dur_ns))

    # --- calculate global sizes and displacements (collective) ---
    (vec_num, vec_displace) = calc_displace(Int64(n_local))
    sum_n = sum(vec_num)

    # Determine this rank's partition
    if parallel_write()
        start_idx = vec_displace[mpi.rank + 1] + 1
        last_idx  = start_idx + vec_num[mpi.rank + 1] - 1
    else
        start_idx = 1
        last_idx  = n_local
    end

    # --- create group and write metadata ---
    gid = create_group(fid, "instrumentation")
    attrs(gid)["n_events_per_rank"] = vec_num
    attrs(gid)["displace"] = vec_displace
    attrs(gid)["mpi_size"] = mpi.size

    # --- write one partitioned HDF5 compound dataset ---
    _inst_write_events(gid, "events", sum_n, events, start_idx, last_idx)

    flush(fid)
    nothing
end


"""Write a partition of the isbits events to a compound dataset."""
function _inst_write_events(gid, name, sum_n, events, start_idx, last_idx)
    if sum_n == 0
        dt = HDF5.Datatype(HDF5.hdf5_type_id(InstEvent))
        create_dataset(gid, name, dt, dataspace((0,)); mpio_mode()...)
        return nothing
    end
    dset = new_dset(gid, name, InstEvent, sum_n, events)
    if !isempty(events)
        dset[start_idx:last_idx] = events
    end
    nothing
end

# ---------------------------------------------------------------------------
# load_instrumentation (for post-analysis)
# ---------------------------------------------------------------------------

"""Rebuild a `VString{32}` from the nested named tuple that HDF5.jl
returns for the `SVector` field of a compound dataset."""
function _inst_vstring_from_nt(nt)
    VString{32}(UInt8[values(nt.data)...])
end

"""Convert one raw row of the `/instrumentation/events` dataset (HDF5.jl
returns generic named tuples for compound datasets) back to an
`InstEvent`. The enum fields are reconstructed from their `Int32`
representation."""
function _inst_event_from_nt(r)
    InstEvent(EventLabel(r.label),
              _inst_vstring_from_nt(r.caller),
              _inst_vstring_from_nt(r.type),
              EventContext(r.context),
              EventKind(r.kind),
              Int32(r.rank),
              Int64(r.step),
              r.transition_nr,
              r.items,
              r.diag,
              r.start_ns,
              r.dur_ns,
              r.send_items,
              r.recv_items,
              r.send_bytes,
              r.recv_bytes,
              r.agent_min_ns,
              r.agent_max_ns,
              r.agent_sum_ns,
              r.agent_sum_sq_hi,
              r.agent_sum_sq_lo)
end

"""
    load_instrumentation(filename)

Loads the instrumentation event stream from an HDF5 file (as written by
[`write_instrumentation!`](@ref)) and returns a `Vector{InstEvent}`
sorted by start time, using the same order as the save
(`(start_ns, -dur_ns)`). Each event carries the `rank` of the process
that recorded it.

The loader returns all ranks' data combined. Outside an MPI run it reads the
file serially. When called during an MPI run on a shared parallel-HDF5 file,
it must be called collectively by all ranks.

`filename` is the base name (without `.h5`); the function handles both the
parallel (single file) and non-parallel (per-rank files `_0.h5`, `_1.h5`, …)
layouts.
"""
function load_instrumentation(filename)
    # Reuse open_h5file to handle the path logic
    fids = open_h5file(nothing, filename)
    if length(fids) == 0
        error("No HDF5 file found for $(filename)")
    end

    # Determine if we need to merge per-rank files
    is_parallel = attrs(fids[1])["HDF5parallel"]

    # Read from /instrumentation/
    # In parallel mode: all data is in one file, read the full datasets
    # In non-parallel mode: data is split across per-rank files
    raw = NamedTuple[]
    if is_parallel || length(fids) == 1
        append!(raw, fids[1]["instrumentation"]["events"][:])
    else
        # Non-parallel: concatenate per-rank data
        for fid in fids
            if haskey(fid, "instrumentation")
                append!(raw, fid["instrumentation"]["events"][:])
            end
        end
        if isempty(raw)
            error("No /instrumentation/ group found in any file for $(filename)")
        end
    end

    foreach(close, fids)
    events = InstEvent[_inst_event_from_nt(r) for r in raw]
    sort!(events, by = e -> (e.start_ns, -e.dur_ns))
    events
end
