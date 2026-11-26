# Compare four ways to replicate records into cell order on every node.
# Usage: benchmark_cell_list_exchange.jl [RECORDS_PER_NODE [CELLS [TRIALS [--simulate RANKS_PER_NODE]]]]
using MPI, Random, Statistics

const API = MPI.API
const Count = API.MPI_Count
const Aint = API.MPI_Aint

"""
    cell(record)

Return the one-based cell number of a synthetic `UInt64` record. The upper
32 bits store a zero-based cell number; the lower 32 bits store a unique ID.
"""
cell(record) = Int(record >> 32) + 1

"""
    gather_variable!(send, recv, counts, leaders_comm)

Gather each rank's `send` vector into the preallocated `recv` vector on every
rank in `leaders_comm`. `counts` gives the number of elements sent by each rank,
in communicator rank order. The resulting blocks are contiguous in `recv`.
"""
function gather_variable!(send, recv, counts, leaders_comm)
    # Allgatherv uses one contiguous block per sender. Its displacements
    # are zero-based and measured in elements of the receive datatype.
    displs = Aint.(cumsum(counts) .- counts)
    datatype = MPI.Datatype(eltype(send))
    API.MPI_Allgatherv_c(
        send, length(send), datatype, recv, counts, displs, datatype, leaders_comm,
    )
    nothing
end

"""
    occupied_cells(records)

Summarize records already grouped by cell as `(cell, count)` tuples in input
order. Omit empty cells. Each cell must occupy a single contiguous run.
"""
function occupied_cells(records)
    # The input is already grouped by cell, so each run gives one (cell, count).
    metadata = Tuple{Int64,Int64}[]
    i = 1
    while i <= length(records)
        key = cell(records[i])
        j = i + 1
        while j <= length(records) && cell(records[j]) == key
            j += 1
        end
        push!(metadata, (key, j - i))
        i = j
    end
    metadata
end

"""
    gather_and_sort!(send, recv, offsets, counts, leaders_comm)

Receive unsorted node contributions into shared memory, then sort the
records by cell and build the dense cell offsets.
"""
function gather_and_sort!(send, recv, offsets, counts, leaders_comm)
    gather_variable!(send, recv, counts, leaders_comm)
    sort!(recv)

    i = 1
    for key in 1:(length(offsets) - 1)
        offsets[key] = i
        while i <= length(recv) && cell(recv[i]) == key
            i += 1
        end
    end
    offsets[end] = i
    nothing
end

"""
    gather_by_cell!(send, recv, offsets, ncells, leaders_comm)

Exchange occupied-cell counts, write the dense cell offsets, and receive
directly into the final cell ranges. The sender's records must already be
grouped by cell. Free temporary MPI types before returning.
"""
function gather_by_cell!(send, recv, offsets, ncells, leaders_comm)
    # Exchange only occupied-cell counts first, in sender rank order.
    metadata = occupied_cells(send)
    sizes = MPI.Allgather(Count(length(metadata)), leaders_comm)
    all_metadata = similar(metadata, sum(sizes))
    gather_variable!(metadata, all_metadata, sizes, leaders_comm)

    totals = zeros(Int, ncells)
    for (key, count) in all_metadata
        totals[key] += count
    end
    # Publish one-based offsets separately from the zero-based write cursors.
    next = cumsum(totals) .- totals
    offsets[1:end-1] .= next .+ 1
    offsets[end] = length(recv) + 1

    base = MPI.Datatype(UInt64)
    types = MPI.Datatype[]
    start = 1
    for nblocks in sizes
        lengths = Vector{Count}(undef, nblocks)
        displacements = Vector{Aint}(undef, nblocks)
        for block in 1:nblocks
            key, count = all_metadata[start]
            lengths[block] = count
            displacements[block] = next[key] * sizeof(UInt64)
            next[key] += count
            start += 1
        end
        # This receive type maps one sender's contiguous stream to its cell
        # ranges. Block lengths count UInt64s; offsets here are in bytes.
        datatype = MPI.Datatype()
        API.MPI_Type_create_hindexed_c(
            nblocks, lengths, displacements, base, datatype,
        )
        MPI.Types.commit!(datatype)
        push!(types, datatype)
    end

    peers = length(sizes)
    # Send the same local buffer to every leader. Receive one instance of
    # each sender's type; that type already contains the target byte offsets.
    zeros_bytes = zeros(Aint, peers)
    API.MPI_Alltoallw_c(
        send, fill(Count(length(send)), peers), zeros_bytes, fill(base.val, peers),
        recv, ones(Count, peers), zeros_bytes, getfield.(types, :val), leaders_comm,
    )
    # Include type creation, commit, and free in every timed direct exchange:
    # a new snapshot can have different cell counts and offsets.
    foreach(MPI.free, types)
    nothing
end

"""
    gather_node_records(records, node_comm)

Gather unsorted rank-local records into one buffer on node rank zero.
Non-leaders participate in the gather but receive no records.
"""
function gather_node_records(records, node_comm)
    leader = MPI.Comm_rank(node_comm) == 0
    counts = MPI.Gather(Count(length(records)), node_comm; root=0)
    counts = leader ? counts : Count[]
    displs = Aint.(cumsum(counts) .- counts)
    gathered = Vector{UInt64}(undef, sum(counts))
    datatype = MPI.Datatype(UInt64)
    API.MPI_Gatherv_c(
        records, length(records), datatype,
        gathered, counts, displs, datatype, 0, node_comm,
    )
    gathered
end

"""
    shared_window(T, n, node_comm)

Allocate an `n`-element array on node rank zero and expose it to every rank
in `node_comm`. Open the passive access epoch used for publication.
"""
function shared_window(::Type{T}, n, node_comm) where {T}
    leader = MPI.Comm_rank(node_comm) == 0
    win, _ = MPI.Win_allocate_shared(Ptr{T}, leader ? n : 0, node_comm)
    array = MPI.Win_shared_query(Array{T}, win; rank=0)
    API.MPI_Win_lock_all(0, win)
    (win, array)
end

"""
    pack_cell_owners(records, width, nranks, destination_ranks)

Pack records by local cell owner. Destination ranks with the same owner on
different nodes reuse the same range of the send buffer.
"""
function pack_cell_owners(records, width, nranks, destination_ranks)
    counts = zeros(Count, nranks)
    for record in records
        owner = cld(cell(record), width)
        counts[owner] += 1
    end

    displs = Aint.(cumsum(counts) .- counts)
    next = Int.(displs) .+ 1
    send = similar(records)
    for record in records
        owner = cld(cell(record), width)
        send[next[owner]] = record
        next[owner] += 1
    end

    (send, counts[destination_ranks], displs[destination_ranks])
end

"""
    write_counted_cells!(received, shared, offsets, first_cell, last_cell, start)

Count records per owned cell, compute their shared offsets, and place each
record directly into its cell range without sorting.
"""
function write_counted_cells!(received, shared, offsets, first_cell, last_cell, start)
    cursors = zeros(Int, max(0, last_cell - first_cell + 1))
    for record in received
        cursors[cell(record) - first_cell + 1] += 1
    end

    next = start
    for i in eachindex(cursors)
        count = cursors[i]
        offsets[first_cell + i - 1] = next
        cursors[i] = next
        next += count
    end

    for record in received
        i = cell(record) - first_cell + 1
        shared[cursors[i]] = record
        cursors[i] += 1
    end
    nothing
end

"""
    exchange_cell_owners!(packed, shared, offsets, first_cell, last_cell,
                          node_comm, world_comm; counted=false)

Receive all contributions for this rank's cells and write its disjoint ranges
in shared memory. Sort and copy the records by default, or count and place them
without sorting when `counted` is true.
"""
function exchange_cell_owners!(
    packed, shared, offsets, first_cell, last_cell, node_comm, world_comm;
    counted=false,
)
    send, sendcounts, sdispls = packed
    recvcounts = MPI.Alltoall(MPI.UBuffer(sendcounts, 1), world_comm)
    rdispls = Aint.(cumsum(recvcounts) .- recvcounts)
    received = similar(send, sum(recvcounts))
    datatype = MPI.Datatype(eltype(send))
    API.MPI_Alltoallv_c(
        send, sendcounts, sdispls, datatype,
        received, recvcounts, rdispls, datatype, world_comm,
    )

    # Cell ranges follow node rank order. The preceding ranks' counts
    # therefore give the start of this rank's shared payload range.
    node_counts = MPI.Allgather(length(received), node_comm)
    node_rank = MPI.Comm_rank(node_comm)
    start = 1
    for i in 1:node_rank
        start += node_counts[i]
    end

    if counted
        write_counted_cells!(received, shared, offsets, first_cell, last_cell, start)
    else
        sort!(received)
        if !isempty(received)
            copyto!(shared, start, received, 1, length(received))
        end

        i = 1
        for key in first_cell:last_cell
            offsets[key] = start + i - 1
            while i <= length(received) && cell(received[i]) == key
                i += 1
            end
        end
    end
    if node_rank == length(node_counts) - 1
        offsets[end] = length(shared) + 1
    end
    nothing
end

"""
    run_trial!(prepare, exchange!, writes, records, windows, node_comm, world_comm)

Time input preparation, exchange and construction, and shared-memory
publication. Report the maximum for each measurement across ranks.
"""
function run_trial!(prepare, exchange!, writes, records, windows, node_comm, world_comm)
    MPI.Barrier(world_comm)
    start = MPI.Wtime()
    prepared = prepare(records)
    after_prepare = MPI.Wtime()
    if writes
        exchange!(prepared)
    end
    after_exchange = MPI.Wtime()

    if writes
        foreach(MPI.Win_sync, windows)
    end
    MPI.Barrier(node_comm)
    foreach(MPI.Win_sync, windows)
    finish = MPI.Wtime()

    # Non-writers wait for writers at the node barrier. Do not count that
    # wait as publication time, but include it in the total.
    publish = writes ? finish - after_exchange : 0.0
    MPI.Allreduce(
        [finish - start, after_prepare - start,
         after_exchange - after_prepare, publish],
        max, world_comm,
    )
end

"""
    main(args)

Compare four strategies starting from identical unsorted records on each
rank. Accept records per node, cell count, and trial count, followed
optionally by `--simulate RANKS_PER_NODE`. Validate the published records
and offsets on all ranks and report median total and phase times.
"""
function main(args)
    n = length(args) >= 1 ? parse(Int, args[1]) : 100_000
    ncells = length(args) >= 2 ? parse(Int, args[2]) : 4096
    trials = length(args) >= 3 ? parse(Int, args[3]) : 7
    simulate = length(args) == 5 && args[4] == "--simulate"
    @assert length(args) <= 3 || simulate "Expected N CELLS TRIALS [--simulate RANKS_PER_NODE]"
    @assert n > 0 && 0 < ncells <= typemax(UInt32) && trials > 0

    world_comm = MPI.COMM_WORLD
    rank = MPI.Comm_rank(world_comm)
    physical_comm = MPI.Comm_split_type(world_comm, MPI.COMM_TYPE_SHARED, rank)
    node_comm = if simulate
        ranks_per_node = parse(Int, args[5])
        @assert ranks_per_node > 0
        @assert MPI.Comm_size(physical_comm) % ranks_per_node == 0
        physical_rank = MPI.Comm_rank(physical_comm)
        MPI.Comm_split(physical_comm, physical_rank ÷ ranks_per_node, physical_rank)
    else
        physical_comm
    end

    node_rank = MPI.Comm_rank(node_comm)
    leader = node_rank == 0
    leaders_comm = MPI.Comm_split(world_comm, leader ? 0 : nothing, rank)
    nnodes = MPI.Allreduce(Int(leader), +, world_comm)
    @assert nnodes >= 2 "Use at least two nodes or simulate at least two groups"
    @assert n <= div(Int64(typemax(UInt32)), nnodes)
    total = n * nnodes

    # Split the same number of records per node across its ranks.
    node_id = MPI.Bcast(leader ? Int(MPI.Comm_rank(leaders_comm)) : 0, 0, node_comm)
    q, r = divrem(n, MPI.Comm_size(node_comm))
    local_count = q + Int(node_rank < r)
    id_offset = node_id * n + node_rank * q + min(node_rank, r)
    rng = MersenneTwister(rank)
    # Sample cell numbers directly: the benchmark stores no positions and does
    # not measure geometric cell assignment. Pack the cell number and ID into one UInt64.
    records = UInt64[(UInt64(rand(rng, 0:ncells-1)) << 32) | UInt64(id_offset + i)
                     for i in 1:local_count]

    records_win, shared = shared_window(UInt64, total, node_comm)
    offsets_win, offsets = shared_window(Int, ncells + 1, node_comm)
    windows = (records_win, offsets_win)
    if leader
        fill!(shared, 0)
        fill!(offsets, 0)
    end

    nranks = MPI.Comm_size(node_comm)
    min_ranks = MPI.Allreduce(nranks, min, world_comm)
    max_ranks = MPI.Allreduce(nranks, max, world_comm)
    @assert min_ranks == max_ranks "Cell-owner exchange requires equal node sizes"

    destination_ranks = MPI.Allgather(Int(node_rank) + 1, world_comm)
    width = cld(ncells, nranks)
    first_cell = node_rank * width + 1
    last_cell = min((node_rank + 1) * width, ncells)

    counts = fill(Count(n), nnodes)
    gather(records) = gather_node_records(records, node_comm)
    pack(records) = pack_cell_owners(records, width, nranks, destination_ranks)
    reorder!(send) = gather_and_sort!(send, shared, offsets, counts, leaders_comm)
    direct!(send) = gather_by_cell!(sort!(send), shared, offsets, ncells, leaders_comm)
    cell_owners!(packed) = exchange_cell_owners!(
        packed, shared, offsets, first_cell, last_cell, node_comm, world_comm,
    )
    cell_owners_counted!(packed) = exchange_cell_owners!(
        packed, shared, offsets, first_cell, last_cell, node_comm, world_comm;
        counted=true,
    )

    a() = run_trial!(gather, reorder!, leader, records, windows, node_comm, world_comm)
    b() = run_trial!(gather, direct!, leader, records, windows, node_comm, world_comm)
    c() = run_trial!(pack, cell_owners!, true, records, windows, node_comm, world_comm)
    d() = run_trial!(pack, cell_owners_counted!, true, records, windows, node_comm, world_comm)
    variants = (a, b, c, d)
    names = ("reorder", "direct", "cell_owners", "cell_owners_counted")

    # Validate all four layouts from every rank, including non-writers.
    a()
    @assert issorted(shared)
    expected = foldl((h, record) -> hash(record, h), shared; init=UInt(0))
    expected_offsets = copy(offsets)
    MPI.Barrier(world_comm)
    for run in (b, c)
        run()
        @assert issorted(shared)
        @assert foldl((h, record) -> hash(record, h), shared; init=UInt(0)) == expected
        @assert offsets == expected_offsets
        MPI.Barrier(world_comm)
    end

    d()
    @assert offsets == expected_offsets
    canonical = copy(shared)
    for key in 1:ncells
        sort!(view(canonical, offsets[key]:(offsets[key + 1] - 1)))
    end
    @assert foldl((h, record) -> hash(record, h), canonical; init=UInt(0)) == expected
    MPI.Barrier(world_comm)

    samples = ntuple(_ -> Vector{Float64}[], length(variants))
    for trial in 1:trials
        for shift in 0:(length(variants) - 1)
            i = mod1(trial + shift, length(variants))
            push!(samples[i], variants[i]())
        end
    end

    if rank == 0
        println("domains=$nnodes ranks=$(MPI.Comm_size(world_comm)) simulated=$simulate ",
                "records_per_domain=$n cells=$ncells")
        println("strategy total_ms input_prep_ms exchange_build_ms publish_ms")
        for (name, timings) in zip(names, samples)
            times = [1000 * median(sample[i] for sample in timings) for i in 1:4]
            println(name, " ", Base.join(round.(times; digits=3), " "))
        end
    end

    for win in reverse(windows)
        API.MPI_Win_unlock_all(win)
        MPI.free(win)
    end
    if leader
        MPI.free(leaders_comm)
    end
    if simulate
        MPI.free(node_comm)
    end
    MPI.free(physical_comm)
    nothing
end

MPI.Init()
try
    main(ARGS)
catch err
    println(stderr, "Rank ", MPI.Comm_rank(MPI.COMM_WORLD), ": ", sprint(showerror, err))
    flush(stderr)
    MPI.Abort(MPI.COMM_WORLD, 1)
end
