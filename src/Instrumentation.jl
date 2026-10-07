export Instrumentation, InstEvent, CommStats, AgentTimeStats
export enable_instrumentation!


# Send/recv volume of a collective call (actual buffer volume, read right
# before the MPI call). Present only when the event carries communication
# volume.
struct CommStats
    send_items::Int64
    recv_items::Int64
    send_bytes::Int64
    recv_bytes::Int64
    CommStats(si = 0, ri = 0, sb = 0, rb = 0) = new(si, ri, sb, rb)
end

# Per-agent timing statistics of the transition loop (only for
# `:transition_stats`, diagnostic-only). Mean/std are derived during the
# post-processing (n = number of agents):
#   mean = sum_ns / n
#   std  = sqrt(sum_sq_ns / n - (sum_ns / n)^2)
mutable struct AgentTimeStats
    min_ns::Int64
    max_ns::Int64
    sum_ns::Int64
    sum_sq_ns::Int128

    # typemax marks an accumulator to which no agent time has been added yet.
    AgentTimeStats() = new(typemax(Int64), typemin(Int64), 0, 0)
    AgentTimeStats(min_ns, max_ns, sum_ns, sum_sq_ns) =
        new(min_ns, max_ns, sum_ns, sum_sq_ns)
end

@inline function _inst_add_agent_time!(stats::AgentTimeStats, dt::Integer)
    dt_ns = Int64(dt)
    if dt_ns < stats.min_ns
        stats.min_ns = dt_ns
    end
    if dt_ns > stats.max_ns
        stats.max_ns = dt_ns
    end
    stats.sum_ns += dt_ns
    stats.sum_sq_ns += Int128(dt_ns) * Int128(dt_ns)
    stats
end

@inline function _inst_agentstats_or_nothing(stats::AgentTimeStats)
    if stats.min_ns == typemax(Int64)
        nothing
    else
        stats
    end
end

# Diagnostic-only transition statistics. The accumulator doubles as the
# on/off token (`nothing` = diagnostics off), so the transition loops carry
# neither a `diagnostic` flag nor the begin/end bookkeeping explicitly.
function _inst_begin_agent_stats(sim, type::DataType)
    if instrumentation_enabled() && sim.instrumentation.diagnostic
        _inst_begin(sim, :transition_stats, type)
        AgentTimeStats()
    else
        nothing
    end
end

function _inst_end_agent_stats(sim, type::DataType,
                               stats::Union{Nothing, AgentTimeStats})
    if stats !== nothing
        _inst_end(sim, :transition_stats, type;
                  agentstats = _inst_agentstats_or_nothing(stats))
    end
    nothing
end

# Times a transition call. Only the tfunc call is measured; wfunc (state
# write) stays outside (O(1), deterministic). `stats === nothing` is
# loop-invariant, so the union split branch is cheap, and all four
# transition variants pass exactly three arguments to tfunc.
@inline function _inst_transition_call(stats::Union{Nothing, AgentTimeStats},
                                       tfunc, a, b, c)
    if stats === nothing
        tfunc(a, b, c)
    else
        t0 = time_ns()
        result = tfunc(a, b, c)
        _inst_add_agent_time!(stats, time_ns() - t0)
        result
    end
end

# A measured interval. parent durations include child durations.
struct InstEvent
    label::Symbol
    # transition name, constant per record, nothing outside of apply!
    caller::Union{Nothing, Symbol}  
    # record origin: :apply, :mapreduce, :init, :load_balancing,
    # :none (orthogonal to kind)
    context::Symbol
    # agent or edge type, Nothing = not type-specific
    type::DataType                   
    transition_nr::Int64             
    start_ns::Int64            
    duration_ns::Int64
    # local item count (agents, edges, ...)
    items::Int64
    # send/recv volume; nothing for local events
    comm::Union{CommStats, Nothing}
    # per-agent timing statistics; nothing except for :transition_stats
    # (diagnostic-only)
    agentstats::Union{AgentTimeStats, Nothing}  
    # :phase / :barrier / :alltoall / :alltoallv / :allreduce / :win_fence
    kind::Symbol                     

    function InstEvent(label, caller, context, type, transition_nr,
                start_ns, duration_ns, kind;
                items = 0, comm = nothing, agentstats = nothing)
        new(label, caller, context, type, transition_nr, start_ns, duration_ns,
            items, comm, agentstats, kind)
    end
end

mutable struct Instrumentation
    # true: additional barriers before collectives +
    # per-agent timing (distorts the timings!)
    diagnostic::Bool                 
    events::Vector{InstEvent}
    # copied into the events at _inst_end; after apply! -> nothing
    current_caller::Union{Nothing, Symbol}   
    # record context (default :none)
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
"""
function enable_instrumentation!(enable::Bool)
    if enable 
        @eval instrumentation_enabled() = true
    else
        @eval instrumentation_enabled() = false
    end
end

# Stack: begin = push(label, type, time_ns), end = pop -> duration.
# LIFO covers nested phases. label/type come from the stack entry; the
# parameters at end are used for consistency checks only.
@inline function _inst_begin(sim, label, type::DataType = Nothing)
    if instrumentation_enabled()
        push!(sim.instrumentation.stack, (label, type, time_ns()))
    end
    nothing
end

@inline function _inst_end(sim, label, type::DataType = Nothing;
                  kind::Symbol = :phase,
                  si::Integer = 0, ri::Integer = 0,
                  sb::Integer = 0, rb::Integer = 0,
                  items::Integer = 0,
                  agentstats::Union{AgentTimeStats, Nothing} = nothing)
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
        # comm is only populated for MPI volume (nothing otherwise)
        push!(inst.events, InstEvent(label,
                                     inst.current_caller,
                                     inst.current_context,
                                     t,
                                     sim.num_transitions,
                                     t0,
                                     now - t0,
                                     kind;
                                     items = items,
                                     comm = si + ri + sb + rb > 0 ?
                                         CommStats(si, ri, sb, rb) : nothing,
                                     agentstats = agentstats))
    end
    nothing
end

# Wraps a phase in one `label` event (replaces the manual
# `_inst_begin`/…/`_inst_end` pairs). `label`/`type` are given exactly once,
# so begin and end cannot drift apart. Body as do-block:
#
#     _inst_phase(sim, :barrier_pre; kind = :barrier) do
#         MPI.Barrier(MPI.COMM_WORLD)
#     end
#
# NOTE: `continue`, `break`, `return` or a throw inside `f()` skips the
# `_inst_end` (unbalanced stack) — the wrapped call sites in `apply!` are
# plain statements. `items` is evaluated at the call site (before `f()`); for
# the slot counts used in `apply!` the length does not change inside `f()`.
@inline function _inst_phase(f::F, sim, label::Symbol,
                             type::DataType = Nothing;
                             kind::Symbol = :phase,
                             items::Integer = 0) where F
    _inst_begin(sim, label, type)
    result = f()
    _inst_end(sim, label, type; kind = kind, items = items)
    result
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

# Record state: set/reset current_caller/current_context in one place.
# func = :none (default) -> caller stays untouched, only the context
# switches (e.g. mapreduce); Vahana transitions are functions, never
# symbols, so the sentinel is cleanly separable by value.
@inline function _inst_enter!(sim, context, func = :none)
    if instrumentation_enabled()
        inst = sim.instrumentation
        if func !== :none
            inst.current_caller = _inst_caller_label(sim, func)
        end
        inst.current_context = context
    end
    sim
end

# Reset after the top-level block (caller loses its validity,
# context -> :none).
@inline function _inst_reset!(sim)
    if instrumentation_enabled()
        inst = sim.instrumentation
        inst.current_caller = nothing
        inst.current_context = :none
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
            _inst_begin(sim, label)
            MPI.Barrier(MPI.COMM_WORLD)
            _inst_end(sim, label; kind = :barrier)
        end
    end
    nothing
end
