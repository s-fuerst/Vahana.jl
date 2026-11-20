module GeometricSpaceTests

using Test
import Vahana
using Vahana: @mayassert
using StaticArrays: SVector
using Logging: with_logger

Vahana.enable_asserts(true)

include(joinpath(@__DIR__, "..", "src", "GeometricSpace.jl"))

@testset "Valid space bounds" begin
    for T in (Int8, Int16, Int32, Int64)
        @test GridSpace([(typemin(T), typemax(T))]) isa GridSpace
    end

    @test GridSpace([(5, 5)]) isa GridSpace

    for T in (Float16, Float32, Float64)
        @test ContinuousSpace([(T(-1), T(1))]) isa ContinuousSpace
    end

    @test ContinuousSpace([(0, 10)]) isa ContinuousSpace

    @testset "Lossless promotion of bound types" begin
        space = GridSpace(((Int8(0), Int16(100)),))
        @test space isa GridSpace{1,Int16,Euclidean}

        space = ContinuousSpace(((Float32(0), Float64(1)),))
        @test space isa ContinuousSpace{1,Float64,Euclidean}
    end
end

@testset "Invalid space bounds" begin
    for Space in (GridSpace, ContinuousSpace)
        @test_throws ArgumentError Space([])
        @test_throws ArgumentError Space(())
        @test_throws ArgumentError Space(10)
        @test_throws ArgumentError Space([(0,)])
        @test_throws ArgumentError Space([(0, 1, 2)])
        @test_throws ArgumentError Space([[0, 10]])

        @test_throws ArgumentError Space([(2, 1)])
        @test_throws ArgumentError Space([(0, 10), (5, 4)])

        for value in (false, UInt8(0), big(0), big(0.0), 0 // 1, 0im)
            @test_throws ArgumentError Space([(value, 10)])
            @test_throws ArgumentError Space([(0, value)])
        end
    end

    @test_throws ArgumentError ContinuousSpace([(5, 5)])

    for T in (Float16, Float32, Float64)
        @test_throws ArgumentError GridSpace([(T(0), T(10))])

        for value in (T(NaN), T(Inf), T(-Inf))
            @test_throws ArgumentError ContinuousSpace([(value, T(1))])
            @test_throws ArgumentError ContinuousSpace([(T(0), value)])
        end
    end

    @testset "Lossy promotion is rejected" begin
        bounds = ((Int64(9_007_199_254_740_993), 1.0e16),)
        @test_throws ArgumentError ContinuousSpace(bounds)
    end
end

struct UnsupportedMetric <: GeometricMetric end

@testset "Space periodicity" begin
    bounds = [(0, 10), (0, 20)]

    for Space in (GridSpace, ContinuousSpace)
        @testset "$Space" begin
            @test Space(bounds).periodic === (false, false)
            @test Space(bounds; periodic=false).periodic === (false, false)
            @test Space(bounds; periodic=true).periodic === (true, true)

            @test Space(
                bounds; periodic=(true, false)
            ).periodic === (true, false)

            for periodic in ((), (true,), (true, false, true))
                @test_throws ArgumentError Space(bounds; periodic)
            end

            for periodic in ((true, 0), (1, false), (nothing, false))
                @test_throws ArgumentError Space(bounds; periodic)
            end

            for periodic in (0, 1, nothing, [true, false])
                @test_throws ArgumentError Space(bounds; periodic)
            end
        end
    end
end

@testset "Space metrics" begin
    for Space in (GridSpace, ContinuousSpace)
        @testset "$Space" begin
            @test Space([(0, 10)]).metric isa Euclidean

            for Metric in (Euclidean, Cityblock, Chebyshev)
                @test Space([(0, 10)]; metric=Metric()).metric isa Metric
            end

            @test_throws ArgumentError Space(
                [(0, 10)]; metric=UnsupportedMetric()
            )
        end
    end
end

@testset "Position validity" begin
    @testset "Grid boundaries" begin
        for periodic in (false, true, (true, false))
            space = GridSpace([(0, 10), (-5, 5)]; periodic)

            @test geom_isvalidposition(space, (0, -5))
            @test geom_isvalidposition(space, (10, 5))
            @test geom_isvalidposition(space, SVector(5, 0))
            @test geom_isvalidposition(space, [5, 0])

            for pos in ((-1, 0), (11, 0), (5, -6), (5, 6))
                @test !geom_isvalidposition(space, pos)
            end
        end

        space = GridSpace([(typemin(Int8), typemax(Int8))])
        @test geom_isvalidposition(space, (typemin(Int8),))
        @test geom_isvalidposition(space, (typemax(Int8),))

        space = GridSpace([(5, 5)])
        @test geom_isvalidposition(space, (5,))
        @test !geom_isvalidposition(space, (6,))
    end

    @testset "Continuous boundaries and partial periodicity" begin
        for T in (Float16, Float32, Float64)
            space = ContinuousSpace(
                [(0, 10), (-5, 5)]; periodic=(true, false)
            )

            @test geom_isvalidposition(space, (T(0), T(-5)))
            @test geom_isvalidposition(space, (T(5), T(5)))
            @test geom_isvalidposition(space, SVector(T(5), T(0)))
            @test !geom_isvalidposition(space, (T(10), T(0)))
            @test !geom_isvalidposition(space, (T(-1), T(0)))
            @test !geom_isvalidposition(space, (T(5), T(6)))
        end

        space = ContinuousSpace([(0, 10)])
        @test geom_isvalidposition(space, (10.0,))
    end

    @testset "Preconditions with assertions enabled" begin
        grid = GridSpace([(0, 10)])
        continuous = ContinuousSpace([(0, 10)])

        for space in (grid, continuous)
            @test_throws AssertionError geom_isvalidposition(space, ())
        end

        @test_throws AssertionError geom_isvalidposition(grid, (1.0,))
        @test_throws AssertionError geom_isvalidposition(continuous, (1,))

        for value in (NaN, Inf, -Inf)
            @test !geom_isvalidposition(continuous, (value,))
        end
    end
end

@testset "Grid periods must fit the bound type" begin
    for T in (Int8, Int16, Int32, Int64)
        @test GridSpace(
            [(T(1), typemax(T))]; periodic=true
        ) isa GridSpace

        @test_throws ArgumentError GridSpace(
            [(T(0), typemax(T))]; periodic=true
        )

        @test_throws ArgumentError GridSpace(
            [(typemin(T), typemax(T))]; periodic=true
        )

        @test GridSpace(
            [(typemin(T), typemax(T))]; periodic=false
        ) isa GridSpace
    end
end

@testset "Grid wrapping" begin
    space = GridSpace([(0, 9), (0, 9)]; periodic=(true, false))

    @test geom_wrap(space, (32, 5)) === SVector(2, 5)
    @test geom_wrap(space, (-22, 5)) === SVector(8, 5)
    @test geom_wrap(space, (9, 9)) === SVector(9, 9)

    @test_throws ArgumentError geom_wrap(space, (12, 12))
    @test_throws ArgumentError geom_wrap(space, (12, -1))
    @test geom_wrap(space, (12, 12), true) === SVector(2, 9)
    @test geom_wrap(space, (12, -1), true) === SVector(2, 0)

    @testset "Extreme positions and nonzero lower bounds" begin
        for T in (Int8, Int16, Int32, Int64)
            for bounds in (((T(3), T(5)),), ((T(-5), T(-3)),))
                extreme_space = GridSpace(bounds; periodic=true)

                for coordinate in (typemin(T), typemax(T), T(0))
                    lo, hi = bounds[1]
                    R = widen(T)
                    period = R(hi) - R(lo) + one(R)
                    expected = T(
                        R(lo) + mod(R(coordinate) - R(lo), period)
                    )

                    @test geom_wrap(
                        extreme_space, (coordinate,)
                    ) === SVector{1,T}(expected)
                end
            end
        end
    end

    @testset "Single-position periodic axis" begin
        single_position_space = GridSpace([(5, 5)]; periodic=true)

        @test geom_wrap(single_position_space, (typemin(Int),)) === SVector(5)
        @test geom_wrap(single_position_space, (typemax(Int),)) === SVector(5)
    end

    @testset "Preconditions" begin
        @test_throws AssertionError geom_wrap(space, (1,))
        @test geom_wrap(
            space, (Int8(1), Int8(2))
        ) === SVector{2,Int8}(1, 2)

        @test_throws AssertionError geom_wrap(space, (1.0, 2.0))
    end

    narrow_space = GridSpace([(Int8(0), Int8(100))]; periodic=true)

    @test geom_wrap(
        narrow_space, (Int128(200),)
    ) === SVector{1,Int128}(99)
end

@testset "Continuous wrapping" begin
    for T in (Float16, Float32, Float64)
        mixed_space = ContinuousSpace(
            [(0, 10), (-5, 5)]; periodic=(true, false)
        )

        @test geom_wrap(
            mixed_space, (T(32), T(5))
        ) === SVector{2,T}(2, 5)

        @test geom_wrap(
            mixed_space, (T(-22), T(-5))
        ) === SVector{2,T}(8, -5)

        @test geom_wrap(
            mixed_space, (T(10), T(0))
        ) === SVector{2,T}(0, 0)

        @test_throws ArgumentError geom_wrap(
            mixed_space, (T(12), T(6))
        )

        @test geom_wrap(
            mixed_space, (T(12), T(6)), true
        ) === SVector{2,T}(2, 5)

        @test geom_wrap(
            mixed_space, (T(12), T(-6)), true
        ) === SVector{2,T}(2, -5)

        shifted_space = ContinuousSpace([(T(-5), T(-3))]; periodic=true)

        @test geom_wrap(
            shifted_space, (T(0),)
        ) === SVector{1,T}(-4)

        @test geom_wrap(
            shifted_space, (T(-3),)
        ) === SVector{1,T}(-5)

        unit_space = ContinuousSpace([(T(0), T(1))]; periodic=true)

        # The wrapped value can round to the excluded upper bound.
        @test geom_wrap(
            unit_space, (-eps(T) / T(4),)
        ) === SVector{1,T}(0)
    end
end

@testset "Continuous period validation" begin
    extreme_bounds = [(-floatmax(Float64), floatmax(Float64))]

    @test_throws ArgumentError ContinuousSpace(
        extreme_bounds; periodic=true
    )

    @test ContinuousSpace(
        extreme_bounds; periodic=false
    ) isa ContinuousSpace
end

@testset "Continuous wrapping preconditions" begin
    continuous_space = ContinuousSpace([(0, 10)])

    @test_throws AssertionError geom_wrap(continuous_space, ())
    @test_throws AssertionError geom_wrap(continuous_space, (1,))

    for value in (NaN, Inf, -Inf)
        @test_throws ArgumentError geom_wrap(continuous_space, (value,))
    end

    precise_bounds_space = ContinuousSpace([(0.0, 0.1)])

    @test_throws AssertionError geom_wrap(
        precise_bounds_space, (0.05f0,)
    )
end

@testset "Continuous movement" begin
    bounded_space = ContinuousSpace([(0, 10), (0, 10)])

    for T in (Float16, Float32, Float64)
        @test geom_move(
            bounded_space, SVector{2,T}(5, 5), SVector(1.0, -2.0)
        ) === SVector{2,T}(6, 3)

        @test geom_move(
            bounded_space, SVector{2,T}(5, 5), SVector(0.0, 0.0)
        ) === SVector{2,T}(5, 5)

        # The second axis stops the entire movement at t = 0.5.
        @test geom_move(
            bounded_space, SVector{2,T}(7, 9), SVector(1.0, 2.0)
        ) === SVector{2,T}(7.5, 10)

        @test geom_move(
            bounded_space, SVector{2,T}(3, 1), SVector(-1.0, -2.0)
        ) === SVector{2,T}(2.5, 0)

        # An outward movement at the boundary prevents all movement.
        @test geom_move(
            bounded_space, SVector{2,T}(0, 5), SVector(-1.0, 2.0)
        ) === SVector{2,T}(0, 5)
    end

    @testset "Periodicity" begin
        periodic_space = ContinuousSpace([(0, 10)]; periodic=true)

        @test geom_move(
            periodic_space, (9.0,), (22.0,)
        ) === SVector(1.0)

        @test geom_move(
            periodic_space, (1.0,), (-22.0,)
        ) === SVector(9.0)

        mixed_space = ContinuousSpace(
            [(0, 10), (0, 10)]; periodic=(true, false)
        )

        # Boundary contact also shortens the periodic component.
        @test geom_move(
            mixed_space, (9.0, 9.0), (4.0, 2.0)
        ) === SVector(1.0, 10.0)
    end

    @testset "Preconditions and rounding diagnostic" begin
        @test_throws AssertionError geom_move(
            bounded_space, (11.0, 5.0), (0.0, 0.0)
        )

        @test_throws AssertionError geom_move(
            bounded_space, (5.0, 5.0), (1.0,)
        )

        for value in (NaN, Inf, -Inf)
            @test_throws AssertionError geom_move(
                bounded_space, (5.0, 5.0), (value, 0.0)
            )
        end

        rounding_space = ContinuousSpace([(0, 1)]; periodic=true)
        movement = SVector(0.5 - 2.0^-26)

        @test_throws AssertionError geom_move(
            rounding_space, SVector(0.5f0), movement
        )

        @test_throws r"Float64 instead of Float32" geom_move(
            rounding_space, SVector(0.5f0), movement
        )

        @test geom_move(
            rounding_space, SVector(0.5), movement
        ) === SVector(0.9999999850988388)
    end
end

@testset "Grid movement" begin
    for T in (Int8, Int16, Int32, Int64)
        bounded_grid = GridSpace([(T(0), T(10)), (T(0), T(10))])

        @test geom_move(
            bounded_grid, SVector{2,T}(5, 5), (1.0, -2.0)
        ) === SVector{2,T}(6, 3)

        @test geom_move(
            bounded_grid, SVector{2,T}(5, 5), (0, 0)
        ) === SVector{2,T}(5, 5)

        # Stop at t = 0.5, then round 7.5 to 8.
        @test_logs (:warn, "Grid movement requires rounding.") begin
            @test geom_move(
                bounded_grid, SVector{2,T}(7, 9), (1.0, 2.0)
            ) === SVector{2,T}(8, 10)
        end

        @test geom_move(
            bounded_grid, SVector{2,T}(0, 5), (-1.0, 2.0)
        ) === SVector{2,T}(0, 5)
    end

    @testset "RoundNearest ties" begin
        rounding_grid = GridSpace([(-10, 10)])

        for (position, movement, expected) in (
            (3, 0.5, 4),
            (2, 0.5, 2),
            (-3, -0.5, -4),
            (-2, -0.5, -2),
        )
            @test_logs (:warn, "Grid movement requires rounding.") begin
                @test geom_move(
                    rounding_grid, (position,), (movement,)
                ) === SVector(expected)
            end
        end
    end

    @testset "Periodicity and intermediate coordinates" begin
        periodic_grid = GridSpace([(0, 9)]; periodic=true)

        @test geom_move(periodic_grid, (9,), (22,)) === SVector(1)
        @test geom_move(periodic_grid, (1,), (-22,)) === SVector(9)

        mixed_grid = GridSpace(
            [(0, 10), (0, 10)]; periodic=(true, false)
        )

        @test geom_move(
            mixed_grid, (9, 9), (4.0, 2.0)
        ) === SVector(0, 10)

        narrow_grid = GridSpace(
            [(Int8(0), Int8(100))]; periodic=true
        )

        @test geom_move(
            narrow_grid, (Int8(100),), (100,)
        ) === SVector{1,Int8}(99)
    end

    @testset "Preconditions" begin
        checked_grid = GridSpace([(0, 10), (0, 10)])

        @test_throws AssertionError geom_move(
            checked_grid, (11, 5), (0, 0)
        )

        @test_throws AssertionError geom_move(
            checked_grid, (5, 5), (1,)
        )

        @test geom_move(
            checked_grid, (Int8(5), Int8(5)), (1, 1)
        ) === SVector{2,Int8}(6, 6)
    end
end

@testset "Grid rounding warning is limited" begin
    warning_grid = GridSpace([(0, 10)])
    logger = Test.TestLogger(; respect_maxlog=true)

    with_logger(logger) do
        for repetition in 1:3
            geom_move(warning_grid, (3,), (0.5,))
        end
    end

    @test length(logger.logs) == 1
    @test logger.logs[1].message == "Grid movement requires rounding."
end

@testset "Grid displacement" begin
    @testset "Safe differences and result types" begin
        for T in (Int8, Int16, Int32, Int64)
            R = widen(T)
            bounded_grid = GridSpace([(typemin(T), typemax(T))])
            expected = R(typemax(T)) - R(typemin(T))

            @test geom_displacement(
                bounded_grid, (typemin(T),), (typemax(T),)
            ) === SVector{1,R}(expected)

            @test geom_displacement(
                bounded_grid, (typemax(T),), (typemin(T),)
            ) === SVector{1,R}(-expected)

            @test geom_displacement(
                bounded_grid, (T(0),), (T(0),)
            ) === SVector{1,R}(0)
        end
    end

    @testset "Minimum image and half-period ties" begin
        periodic_grid = GridSpace([(0, 9)]; periodic=true)

        for (from, to, expected) in (
            (1, 9, -2),
            (0, 5, 5),
            (0, 25, 5),
            (0, -25, -5),
            (1, 29, -2),
            (0, 30, 0),
        )
            @test geom_displacement(
                periodic_grid, (from,), (to,)
            ) == SVector(expected)

            @test geom_displacement(
                periodic_grid, (to,), (from,)
            ) == SVector(-expected)
        end

        odd_period_grid = GridSpace([(0, 8)]; periodic=true)

        @test geom_displacement(
            odd_period_grid, (0,), (5,)
        ) == SVector(-4)

        @test geom_displacement(
            odd_period_grid, (5,), (0,)
        ) == SVector(4)
    end

    @testset "Mixed periodicity and coordinate types" begin
        mixed_grid = GridSpace(
            [(0, 9), (0, 20)]; periodic=(true, false)
        )

        @test geom_displacement(
            mixed_grid, (1, 1), (9, 19)
        ) == SVector(-2, 18)

        narrow_grid = GridSpace([(Int8(0), Int8(9))]; periodic=true)

        @test geom_displacement(
            narrow_grid, (Int8(1),), (Int16(9),)
        ) === SVector{1,Int32}(-2)
    end

    @testset "Preconditions" begin
        checked_grid = GridSpace([(0, 9)])

        @test_throws AssertionError geom_displacement(
            checked_grid, (), (1,)
        )

        @test_throws AssertionError geom_displacement(
            checked_grid, (1,), ()
        )

        @test_throws AssertionError geom_displacement(
            checked_grid, (1.0,), (2,)
        )

        @test_throws AssertionError geom_displacement(
            checked_grid, (1,), (2.0,)
        )
    end
end

@testset "Continuous displacement" begin
    @testset "Coordinate types and ordinary differences" begin
        bounded_space = ContinuousSpace([(-2000, 2000)])

        for T in (Float16, Float32, Float64)
            @test geom_displacement(
                bounded_space, (T(-1000),), (T(1000),)
            ) === SVector{1,Float64}(2000)

            @test geom_displacement(
                bounded_space, (T(1000),), (T(-1000),)
            ) === SVector{1,Float64}(-2000)

            @test geom_displacement(
                bounded_space, (T(1),), (T(1),)
            ) == SVector(0.0)
        end
    end

    @testset "Minimum image and half-period ties" begin
        periodic_space = ContinuousSpace([(0, 10)]; periodic=true)

        for T in (Float16, Float32, Float64)
            for (from, to, expected) in (
                (1, 9, -2),
                (0, 5, 5),
                (0, 25, 5),
                (0, -25, -5),
                (1, 29, -2),
                (0, 30, 0),
            )
                @test geom_displacement(
                    periodic_space, (T(from),), (T(to),)
                ) == SVector(Float64(expected))

                @test geom_displacement(
                    periodic_space, (T(to),), (T(from),)
                ) == SVector(Float64(-expected))
            end
        end
    end

    @testset "Mixed periodicity and shifted bounds" begin
        mixed_space = ContinuousSpace(
            [(0, 10), (0, 20)]; periodic=(true, false)
        )

        @test geom_displacement(
            mixed_space, (1.0, 1.0), (9.0, 19.0)
        ) === SVector(-2.0, 18.0)

        for bounds in ([(5, 15)], [(-5, 5)])
            shifted_space = ContinuousSpace(bounds; periodic=true)
            lo, hi = bounds[1]

            @test geom_displacement(
                shifted_space, (Float64(lo + 1),), (Float64(hi - 1),)
            ) == SVector(-2.0)
        end

        @test geom_displacement(
            mixed_space,
            SVector{2,Float16}(1, 1),
            SVector{2,Float32}(9, 19),
        ) === SVector{2,Float64}(-2, 18)
    end

    @testset "Preconditions" begin
        checked_space = ContinuousSpace([(0, 10)])

        @test_throws AssertionError geom_displacement(
            checked_space, (), (1.0,)
        )

        @test_throws AssertionError geom_displacement(
            checked_space, (1.0,), ()
        )

        @test_throws AssertionError geom_displacement(
            checked_space, (1,), (2.0,)
        )

        @test_throws AssertionError geom_displacement(
            checked_space, (1.0,), (2,)
        )
    end
end

@testset "Geometric distance" begin
    for (Metric, expected) in (
        (Euclidean, 5),
        (Cityblock, 7),
        (Chebyshev, 4),
    )
        @testset "$Metric" begin
            grid = GridSpace([(0, 9), (0, 9)]; metric=Metric())
            distance = geom_distance(
                grid, SVector{2,Int8}(1, 1), SVector{2,Int8}(4, 5)
            )

            @test distance == expected
            if Metric == Euclidean
                @test distance isa Float64
            else
                @test distance isa Int128
            end

            continuous = ContinuousSpace(
                [(0, 10), (0, 10)]; metric=Metric()
            )
            @test geom_distance(
                continuous, SVector{2,Float32}(1, 1),
                SVector{2,Float32}(4, 5)
            ) === Float64(expected)

            for (Space, upper, a, b) in (
                (GridSpace, 9, SVector(1, 1), SVector(9, 8)),
                (ContinuousSpace, 10, SVector(1.0, 1.0), SVector(9.0, 8.0)),
            )
                periodic = Space(
                    [(0, upper), (0, upper)];
                    periodic=true, metric=Metric(),
                )

                distance = geom_distance(periodic, a, b)
                if Metric == Euclidean
                    @test distance ≈ sqrt(13.0)
                elseif Metric == Cityblock
                    @test distance == 5
                else
                    @test distance == 3
                end

                @test geom_distance(periodic, b, a) ≈ distance
                @test iszero(geom_distance(periodic, a, a))
            end

            wide_grid = GridSpace([(-128, 127)]; metric=Metric())
            @test geom_distance(
                wide_grid, SVector(Int8(-128)), SVector(Int8(127))
            ) == 255
        end
    end

    @test _geom_distance(
        Cityblock(), SVector(typemax(Int64) - 1, Int64(1))
    ) === typemax(Int64)

    @test_throws AssertionError _geom_distance(
        Cityblock(), SVector(typemax(Int64), Int64(1))
    )
end

end
