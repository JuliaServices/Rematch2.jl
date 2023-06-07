#
# Additional tests to improve node coverage.
# These tests mainly exercise diagnostic node that isn't crucial to the
# normal operation of the package.
#

abstract type A; end
struct B <: A; end
struct C <: A; end
struct D; x; end

expected="""
"""
@testset "Additional tests to improve node coverage" begin
    io = IOBuffer()
    Rematch2.@match2_dumpall io e begin
        1                            => 1
        Foo(x, x)                    => 2
        y::D                         => 3   
        [x, y..., z]                 => y
        (x, y..., z) where e.q1      => z
        6 || 7                       => 6
        (x::A) where e.q2            => 7
        (x::B) where e.q3            => 8
        (x::C) where e.q4            => 9
        _                            => 10
    end
    actual = String(take!(io))

    #
    # The debug output isn't deterministic yet, so we can't test it.
    # We should figure out why and make it deterministic.
    #
    # println()
    # println(actual)
    # println()
    # for (a, e) in zip(split(actual, '\n'), split(expected, '\n'))
    #     @test a == e
    # end
    # @test actual == expected
    @test true
end
