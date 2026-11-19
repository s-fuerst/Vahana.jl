export Euclidean, Cityblock, Chebyshev
export GeometricSpace, GridSpace, ContinuousSpace
export geom_isvalidposition, geom_wrap, geom_move, geom_displacement

using StaticArrays: SVector

abstract type GeometricMetric end

struct Euclidean <: GeometricMetric end
struct Cityblock <: GeometricMetric end
struct Chebyshev <: GeometricMetric end

const SupportedGridCoordinate = Union{Int8,Int16,Int32,Int64}
const SupportedContinuousCoordinate = Union{Float16,Float32,Float64}
const SupportedBound = Union{
    SupportedGridCoordinate,
    SupportedContinuousCoordinate,
}

abstract type GeometricSpace end

struct GridSpace{N,B,M<:GeometricMetric} <: GeometricSpace
    bounds::NTuple{N,Tuple{B,B}}
    periodic::NTuple{N,Bool}
    metric::M

    function GridSpace(
        bounds::NTuple{N,Tuple{B,B}},
        periodic::NTuple{N,Bool},
        metric::M,
    ) where {N,B,M<:GeometricMetric}
        _validate_geom_space(bounds, metric, true)
        _validate_grid_periods(bounds, periodic)
        new{N,B,M}(bounds, periodic, metric)
    end
end

function GridSpace(
    bounds;
    periodic=false,
    metric=Euclidean(),
)
    bounds_normalized = _geom_bounds(bounds)
    periodic_normalized = _geom_periodic(periodic, length(bounds_normalized))

    GridSpace(bounds_normalized, periodic_normalized, metric)
end

struct ContinuousSpace{N,B,M<:GeometricMetric} <: GeometricSpace
    bounds::NTuple{N,Tuple{B,B}}
    periodic::NTuple{N,Bool}
    metric::M

    function ContinuousSpace(
        bounds::NTuple{N,Tuple{B,B}},
        periodic::NTuple{N,Bool},
        metric::M,
    ) where {N,B,M<:GeometricMetric}
        _validate_geom_space(bounds, metric, false)
        _validate_continuous_periods(bounds, periodic)
        new{N,B,M}(bounds, periodic, metric)
    end
end

function ContinuousSpace(
    bounds;
    periodic=false,
    metric=Euclidean(),
)
    bounds_normalized = _geom_bounds(bounds)
    periodic_normalized = _geom_periodic(periodic, length(bounds_normalized))

    ContinuousSpace(bounds_normalized, periodic_normalized, metric)
end

function _geom_bounds(bounds)
    if !(bounds isa Union{Tuple,AbstractVector})
        throw(ArgumentError(
            "Bounds must be a tuple or vector of pairs."
        ))
    end

    if isempty(bounds)
        throw(ArgumentError(
            "A geometric space needs at least one dimension."
        ))
    end

    for (axis, pair) in enumerate(bounds)
        if !(pair isa Tuple)
            throw(ArgumentError(
                "Bounds on axis $axis must be a tuple (lower, upper)."
            ))
        end

        if length(pair) != 2
            throw(ArgumentError(
                "Bounds on axis $axis must contain exactly two values."
            ))
        end

        for value in pair
            if !(value isa SupportedBound)
                throw(ArgumentError(
                    "Unsupported bound type on axis $axis: $(typeof(value))."
                ))
            end
        end
    end

    bound_type = promote_type(
        (typeof(value) for pair in bounds for value in pair)...
    )

    bounds_normalized = Tuple(map(bounds) do pair
        map(pair) do value
            value_normalized = convert(bound_type, value)

            if !isequal(value_normalized, value)
                throw(ArgumentError(
                    "Converting bounds to $bound_type would lose precision."
                ))
            end

            value_normalized
        end
    end)

    bounds_normalized
end

function _geom_periodic(periodic, dimensions::Int)
    if periodic isa Bool
        periodic_normalized = ntuple(_ -> periodic, dimensions)
    elseif periodic isa Tuple
        if length(periodic) != dimensions
            throw(ArgumentError(
                "Periodicity must contain $dimensions values."
            ))
        end

        if !all(value -> value isa Bool, periodic)
            throw(ArgumentError(
                "Periodicity values must be Bool."
            ))
        end

        periodic_normalized = periodic
    else
        throw(ArgumentError(
            "Periodicity must be a Bool or a tuple of Bool values."
        ))
    end

    periodic_normalized
end

function geom_wrap(space::GridSpace{N,B}, pos, clamp_nonperiodic::Bool=false) where {N,B}
    T = eltype(pos)

    @mayassert length(pos) == N
    @mayassert T <: Union{SupportedGridCoordinate,Int128}

    R = promote_type(T, B)

    coordinates = ntuple(N) do axis
        lo = R(space.bounds[axis][1])
        hi = R(space.bounds[axis][2])
        coordinate = R(pos[axis])

        if space.periodic[axis]
            period = hi - lo + one(R)
            offset = mod(coordinate, period) - mod(lo, period)

            if offset < zero(R)
                offset += period
            end

            B(lo + offset)
        elseif clamp_nonperiodic
            B(clamp(coordinate, lo, hi))
        else
            if !(lo <= coordinate <= hi)
                throw(ArgumentError(
                    "Position exceeds nonperiodic bounds on axis $axis."
                ))
            end

            B(coordinate)
        end
    end

    SVector{N,B}(coordinates)
end

function geom_wrap(space::ContinuousSpace{N}, pos, clamp_nonperiodic::Bool=false) where {N}
    T = eltype(pos)

    @mayassert length(pos) == N
    @mayassert T <: SupportedContinuousCoordinate
    @mayassert all(
        bound -> isequal(T(bound[1]), bound[1]) &&
                 isequal(T(bound[2]), bound[2]),
        space.bounds,
    )

    coordinates = ntuple(N) do axis
        lo = Float64(space.bounds[axis][1])
        hi = Float64(space.bounds[axis][2])
        coordinate = Float64(pos[axis])

        if space.periodic[axis]
            period = hi - lo
            offset = mod(coordinate, period) - mod(lo, period)

            if offset < 0
                offset += period
            end

            wrapped = T(lo + offset)

            # Conversion to T can round up to the excluded upper bound.
            # Use the periodically equivalent lower bound in that case.
            if wrapped >= hi
                T(lo)
            else
                wrapped
            end
        elseif clamp_nonperiodic
            T(clamp(coordinate, lo, hi))
        else
            if !(lo <= coordinate <= hi)
                throw(ArgumentError(
                    "Position exceeds nonperiodic bounds on axis $axis."
                ))
            end

            pos[axis]
        end
    end

    SVector{N,T}(coordinates)
end

function geom_move(space::GridSpace{N,B}, pos, move_vector) where {N,B}
    R = widen(B)

    @mayassert eltype(pos) == B
    @mayassert geom_isvalidposition(space, pos)
    @mayassert length(move_vector) == N

    t = 1.0

    for axis in 1:N
        if !space.periodic[axis]
            lo, hi = space.bounds[axis]
            coordinate = Float64(pos[axis])
            movement = Float64(move_vector[axis])

            if movement > 0
                t = min(t, (Float64(hi) - coordinate) / movement)
            elseif movement < 0
                t = min(t, (Float64(lo) - coordinate) / movement)
            end
        end
    end

    travel_fraction = t

    target = SVector{N,R}(ntuple(N) do axis
        target_coordinate = muladd(
            travel_fraction,
            Float64(move_vector[axis]),
            Float64(pos[axis]),
        )
        rounded = round(target_coordinate, RoundNearest)

        if rounded != target_coordinate
            @warn "Grid movement requires rounding." maxlog=1
        end

        R(rounded)
    end)

    geom_wrap(space, target, true)
end

function geom_move(space::ContinuousSpace{N}, pos, move_vector) where {N}
    T = eltype(pos)

    @mayassert geom_isvalidposition(space, pos)
    @mayassert length(move_vector) == N

    t = 1.0

    for axis in 1:N
        if !space.periodic[axis]
            lo, hi = space.bounds[axis]
            coordinate = Float64(pos[axis])
            movement = Float64(move_vector[axis])

            if movement > 0
                t = min(t, (Float64(hi) - coordinate) / movement)
            elseif movement < 0
                t = min(t, (Float64(lo) - coordinate) / movement)
            end
        end
    end

    travel_fraction = t

    target = SVector{N,Float64}(ntuple(N) do axis
        muladd(travel_fraction, Float64(move_vector[axis]), Float64(pos[axis]))
    end)

    target_normalized = geom_wrap(space, target, true)
    result = SVector{N,T}(target_normalized)

    @mayassert geom_isvalidposition(space, result) (
        "Movement result is invalid after conversion to $T. " *
        "Use a higher-precision position coordinate type, " *
        "for example Float64 instead of Float32."
    )

    result
end

function geom_displacement(space::GridSpace{N,B}, from, to) where {N,B}
    @mayassert length(from) == N && length(to) == N
    @mayassert eltype(from) <: SupportedGridCoordinate
    @mayassert eltype(to) <: SupportedGridCoordinate

    R = widen(promote_type(eltype(from), eltype(to), B))

    coordinates = ntuple(N) do axis
        delta = R(to[axis]) - R(from[axis])

        if space.periodic[axis]
            lo, hi = space.bounds[axis]
            period = R(hi) - R(lo) + one(R)
            reduced = rem(delta, period)
            half_period = period ÷ 2

            if reduced > half_period
                reduced - period
            elseif reduced < -half_period
                reduced + period
            else
                reduced
            end
        else
            delta
        end
    end

    SVector{N,R}(coordinates)
end

function geom_displacement(space::ContinuousSpace{N}, from, to) where {N}
    @mayassert length(from) == N && length(to) == N
    @mayassert eltype(from) <: SupportedContinuousCoordinate
    @mayassert eltype(to) <: SupportedContinuousCoordinate

    coordinates = ntuple(N) do axis
        delta = Float64(to[axis]) - Float64(from[axis])

        if space.periodic[axis]
            lo, hi = space.bounds[axis]
            period = Float64(hi) - Float64(lo)
            reduced = rem(delta, period)
            half_period = period / 2

            if reduced > half_period
                reduced - period
            elseif reduced < -half_period
                reduced + period
            else
                reduced
            end
        else
            delta
        end
    end

    SVector{N,Float64}(coordinates)
end

function geom_isvalidposition(space::GridSpace{N}, pos) where {N}
    @mayassert length(pos) == N
    @mayassert all(value -> value isa SupportedGridCoordinate, pos)

    valid = true

    @inbounds for axis in 1:N
        lo, hi = space.bounds[axis]
        coordinate = pos[axis]

        valid &= lo <= coordinate <= hi
    end

    valid
end

function geom_isvalidposition(space::ContinuousSpace{N}, pos) where {N}
    @mayassert length(pos) == N
    @mayassert all(value -> value isa SupportedContinuousCoordinate, pos)

    valid = true

    @inbounds for axis in 1:N
        lo, hi = space.bounds[axis]
        coordinate = pos[axis]

        if space.periodic[axis]
            inside = lo <= coordinate < hi
        else
            inside = lo <= coordinate <= hi
        end

        valid &= inside
    end

    valid
end

function _validate_geom_space(bounds, metric, grid::Bool)
    if isempty(bounds)
        throw(ArgumentError(
            "A geometric space needs at least one dimension."
        ))
    end

    if !(metric isa Union{Euclidean,Cityblock,Chebyshev})
        throw(ArgumentError(
            "Supported metrics are Euclidean, Cityblock and Chebyshev."
        ))
    end

    for (axis, (lo, hi)) in enumerate(bounds)
        if grid
            if !(lo isa SupportedGridCoordinate &&
                 hi isa SupportedGridCoordinate)
                throw(ArgumentError(
                    "Grid bounds on axis $axis must use supported signed integers."
                ))
            end
        else
            if !(lo isa SupportedBound && hi isa SupportedBound)
                throw(ArgumentError(
                    "Continuous bounds on axis $axis must use supported real types."
                ))
            end
        end

        if !(isfinite(lo) && isfinite(hi))
            throw(ArgumentError(
                "Bounds on axis $axis must be finite."
            ))
        end

        if grid
            if lo > hi
                throw(ArgumentError(
                    "Grid bounds on axis $axis require lower <= upper."
                ))
            end
        else
            if lo >= hi
                throw(ArgumentError(
                    "Continuous bounds on axis $axis require lower < upper."
                ))
            end
        end
    end

    nothing
end

function _validate_grid_periods(
    bounds::NTuple{N,Tuple{B,B}},
    periodic::NTuple{N,Bool},
) where {N,B}
    R = widen(B)

    for axis in 1:N
        if periodic[axis]
            lo, hi = bounds[axis]
            period = R(hi) - R(lo) + one(R)

            if period > typemax(B)
                throw(ArgumentError(
                    "Grid period on axis $axis is not representable as $B."
                ))
            end
        end
    end

    nothing
end

function _validate_continuous_periods(bounds, periodic)
    for axis in eachindex(bounds)
        if periodic[axis]
            lo, hi = bounds[axis]
            period = Float64(hi) - Float64(lo)

            if !(isfinite(period) && period > 0)
                throw(ArgumentError(
                    "Continuous period on axis $axis must be " *
                    "positive and finite in Float64."
                ))
            end
        end
    end

    nothing
end
