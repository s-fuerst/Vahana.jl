# Example: record Vahana instrumentation in a PARALLEL run, save it to
# HDF5, load it back and convert it to a DataFrames.DataFrame — including
# the MPI rank of each event as a column.
#
# Run from the Vahana project root (so that h5/ is found), e.g. with 3 ranks:
#   mpiexecjl -n 3 julia --project=. examples/instrumentation_dataframe.jl
# (a plain `julia --project=. ...` works too, i.e. with a single rank)
#
# Step 1 (simulation) creates the test file:
#   - parallel HDF5:   h5/InstrumentationExample.h5        (one shared file)
#   - without:         h5/InstrumentationExample_<rank>.h5 (one file per rank)
# Step 2 (loading) reads it back, independent of any running simulation —
# re-running the script against an existing file only does step 2.

using Vahana
using Vahana: InstEvent
using DataFrames
using Printf

suppress_warnings(true)

# ---------------------------------------------------------------------------
# Helpers: Vector{InstEvent} -> DataFrame
# ---------------------------------------------------------------------------

# VString{32} fields: fixed-width byte strings, NUL-padded; empty = "not
# set". (Converted manually: the `String(VString{N})` constructor only
# exists in processes where `enable_instrumentation!` created it.)
function vstring_val(s)
    b = collect(s)
    i = findfirst(iszero, b)
    i === nothing ? String(b) : (i == 1 ? nothing : String(b[1:i-1]))
end

# Int128 is stored hi/lo in the event (HDF5 has no Int128)
@inline sum_sq(e) = Vahana._inst_int128_reassemble(e.agent_sum_sq_hi,
                                                   e.agent_sum_sq_lo)

"""Convert a `Vector{InstEvent}` (e.g. from `load_instrumentation`) to a
DataFrame. Enum columns are stored as their Symbol names; `rank` is the
MPI rank that recorded the event."""
function inst_events_df(events::Vector{InstEvent})
    DataFrame(
        # NOTE: explicit per-element Symbol(...) — `Symbol[<strings>]` goes
        # through convert(Symbol, ::String), which does not exist
        label          = [Symbol(string(e.label)) for e in events],
        context        = [Symbol(string(e.context)) for e in events],
        kind           = [Symbol(string(e.kind)) for e in events],
        rank           = [e.rank for e in events],
        caller         = [vstring_val(e.caller) for e in events],
        type           = [vstring_val(e.type) for e in events],
        transition_nr  = [e.transition_nr for e in events],
        diag           = [e.diag for e in events],
        start_ns       = [e.start_ns for e in events],
        dur_ns         = [e.dur_ns for e in events],
        items          = [e.items for e in events],
        send_items     = [e.send_items for e in events],
        recv_items     = [e.recv_items for e in events],
        send_bytes     = [e.send_bytes for e in events],
        recv_bytes     = [e.recv_bytes for e in events],
        agent_min_ns   = [e.agent_min_ns for e in events],
        agent_max_ns   = [e.agent_max_ns for e in events],
        agent_sum_ns   = [e.agent_sum_ns for e in events],
        agent_sum_sq   = [sum_sq(e) for e in events],
    )
end

const BASENAME = "InstrumentationExample"

struct A1 x::Int64 end
struct E1 end

# ---------------------------------------------------------------------------
# 2) Loading: read the HDF5 file back and convert to a DataFrame.
# Inside an MPI run the load is collective; only rank 0 prints.
# ---------------------------------------------------------------------------
function load_and_show()
    events = load_instrumentation(BASENAME)
    if Vahana.mpi.rank == 0
        println("loaded ", length(events), " events (Vector{InstEvent})")
        df = inst_events_df(events)
        show(df[1:15, :])                  # first rows

        # a few typical analyses
        println()
        println("events per rank:")
        for sub in groupby(df, :rank)
            @printf("  rank %d: %3d events, total %8.3f ms\n",
                    sub.rank[1], nrow(sub), sum(sub.dur_ns) / 1e6)
        end

        println("\ntotal duration per label (ms):")
        for sub in groupby(df, :label)
            @printf("  %-28s %10.3f ms   (%d events)\n",
                    sub.label[1], sum(sub.dur_ns) / 1e6, nrow(sub))
        end

        apply_df = df[df.context .== :EC_apply, :]
        println("\napply-context events: ", nrow(apply_df), " / ", nrow(df))
        df
    else
        nothing
    end
end

# ---------------------------------------------------------------------------
# 1) Simulation: record events and write them to h5/<BASENAME>*.h5
#
# NOTE: the simulation code runs at top level, not inside a function:
# create_model generates new methods (world age), and a function defined
# earlier in the script would still be compiled for the old world.
# ---------------------------------------------------------------------------
file_exists = isfile("h5/" * BASENAME * ".h5") ||
    isfile("h5/" * BASENAME * "_0.h5")
if !file_exists
    println("no h5/$(BASENAME)*.h5 found -> running the example simulation")
    # sequential calls instead of `|>`: register_agenttype!/register_edgetype!
    # generate methods at runtime, and the inlined pipe closure would be
    # compiled for a world that predates them (world age)
    model = ModelTypes()
    register_agenttype!(model, A1)
    register_edgetype!(model, E1)
    model = create_model(model, "Instrumentation Example")
    sim = create_simulation(model; filename = BASENAME)
    add_agents!(sim, [A1(i) for i in 1:50])
    finish_init!(sim)                      # init phase events (distribute!)

    enable_instrumentation!(true)
    apply!(sim, (s, id, sim) -> A1(s.x + 1), A1, [A1], [A1, E1])
    apply!(sim, (s, id, sim) -> A1(s.x * 2), A1, [A1], [A1, E1])
    enable_instrumentation!(false)

    println("rank ", Vahana.mpi.rank,
            " recorded ", length(sim.instrumentation.events), " events")
    write_instrumentation!(sim)
    close_h5file!(sim)
end

# ---------------------------------------------------------------------------
# 3) Loading: read the HDF5 file back and convert to a DataFrame
# ---------------------------------------------------------------------------
load_and_show()
