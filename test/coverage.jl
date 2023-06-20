#
# Additional tests to improve code coverage.
# These tests mainly exercise diagnostic code that isn't crucial to the
# normal operation of the package.
#

abstract type A; end
struct B <: A; end
struct C <: A; end
struct D; x; end

expected="""

State Machine: (39 states) input «input_value»
State 1
  1: «input_value» == 1 => 1
  2: («input_value» isa Main.Rematch2Tests.Foo && «input_value.x» := «input_value».x && «input_value.y» := «input_value».y && «input_value.y» == «input_value.x») => 2
  3: «input_value» isa Main.Rematch2Tests.D => 3
  4: («input_value» isa AbstractArray && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» == 1
    THEN: State 2 (fall through)
    ELSE: State 3
State 2
  1: true => 1
    MATCH 1 with value 1
    END
State 3
  2: («input_value» isa Main.Rematch2Tests.Foo && «input_value.x» := «input_value».x && «input_value.y» := «input_value».y && «input_value.y» == «input_value.x») => 2
  3: «input_value» isa Main.Rematch2Tests.D => 3
  4: («input_value» isa AbstractArray && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» isa Main.Rematch2Tests.Foo
    THEN: State 4 (fall through)
    ELSE: State 8
State 4
  2: («input_value.x» := «input_value».x && «input_value.y» := «input_value».y && «input_value.y» == «input_value.x») => 2
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    FETCH «input_value.x» := «input_value».x
    NEXT: State 5 (fall through)
State 5
  2: («input_value.y» := «input_value».y && «input_value.y» == «input_value.x») => 2
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    FETCH «input_value.y» := «input_value».y
    NEXT: State 6 (fall through)
State 6
  2: «input_value.y» == «input_value.x» => 2
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    TEST «input_value.y» == «input_value.x»
    THEN: State 7 (fall through)
    ELSE: State 22
State 7
  2: true => 2
    MATCH 2 with value 2
    END
State 8
  3: «input_value» isa Main.Rematch2Tests.D => 3
  4: («input_value» isa AbstractArray && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» isa Main.Rematch2Tests.D
    THEN: State 9 (fall through)
    ELSE: State 10
State 9
  3: true => 3
    MATCH 3 with value 3
    END
State 10
  4: («input_value» isa AbstractArray && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» isa AbstractArray
    THEN: State 11 (fall through)
    ELSE: State 15
State 11
  4: («length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)]) => «input_value[2:(length-1)]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    FETCH «length(input_value)» := length(«input_value»)
    NEXT: State 12 (fall through)
State 12
  4: («length(input_value)» >= 2 && «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)]) => «input_value[2:(length-1)]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    TEST «length(input_value)» >= 2
    THEN: State 13 (fall through)
    ELSE: State 22
State 13
  4: «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)] => «input_value[2:(length-1)]»
    FETCH «input_value[2:(length-1)]» := «input_value»[2:(length(##input_value#1860)-1)]
    NEXT: State 14 (fall through)
State 14
  4: true => «input_value[2:(length-1)]»
    MATCH 4 with value [y => «input_value[2:(length-1)]»]«input_value[2:(length-1)]»
    END
State 15
  5: («input_value» isa Tuple && «length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» isa Tuple
    THEN: State 16 (fall through)
    ELSE: State 24
State 16
  5: («length(input_value)» := length(«input_value») && «length(input_value)» >= 2 && «input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    FETCH «length(input_value)» := length(«input_value»)
    NEXT: State 17 (fall through)
State 17
  5: («length(input_value)» >= 2 && «input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    TEST «length(input_value)» >= 2
    THEN: State 18 (fall through)
    ELSE: State 22
State 18
  5: («input_value[-1]» := «input_value»[-1] && «where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    FETCH «input_value[-1]» := «input_value»[-1]
    NEXT: State 19 (fall through)
State 19
  5: («where_0» := (e).q1 && where «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    FETCH «where_0» := (e).q1
    NEXT: State 20 (fall through)
State 20
  5: where «where_0» => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    TEST where «where_0»
    THEN: State 21 (fall through)
    ELSE: State 22
State 21
  5: true => «input_value[-1]»
    MATCH 5 with value [z => «input_value[-1]»]«input_value[-1]»
    END
State 22
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: true => 10
    TEST «input_value» == 6
    THEN: State 26
    ELSE: State 23
State 23
  6: «input_value» == 7 => 6
  10: true => 10
    TEST «input_value» == 7
    THEN: State 26
    ELSE: State 39
State 24
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» == 6
    THEN: State 26
    ELSE: State 25
State 25
  6: «input_value» == 7 => 6
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» == 7
    THEN: State 26 (fall through)
    ELSE: State 27
State 26
  6: true => 6
    MATCH 6 with value 6
    END
State 27
  7: («input_value» isa Main.Rematch2Tests.A && «where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» isa Main.Rematch2Tests.A
    THEN: State 28 (fall through)
    ELSE: State 39
State 28
  7: («where_1» := (e).q2 && where «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    FETCH «where_1» := (e).q2
    NEXT: State 29 (fall through)
State 29
  7: where «where_1» => 7
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST where «where_1»
    THEN: State 30 (fall through)
    ELSE: State 31
State 30
  7: true => 7
    MATCH 7 with value 7
    END
State 31
  8: («input_value» isa Main.Rematch2Tests.B && «where_2» := (e).q3 && where «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» isa Main.Rematch2Tests.B
    THEN: State 32 (fall through)
    ELSE: State 35
State 32
  8: («where_2» := (e).q3 && where «where_2») => 8
  10: true => 10
    FETCH «where_2» := (e).q3
    NEXT: State 33 (fall through)
State 33
  8: where «where_2» => 8
  10: true => 10
    TEST where «where_2»
    THEN: State 34 (fall through)
    ELSE: State 39
State 34
  8: true => 8
    MATCH 8 with value 8
    END
State 35
  9: («input_value» isa Main.Rematch2Tests.C && «where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    TEST «input_value» isa Main.Rematch2Tests.C
    THEN: State 36 (fall through)
    ELSE: State 39
State 36
  9: («where_3» := (e).q4 && where «where_3») => 9
  10: true => 10
    FETCH «where_3» := (e).q4
    NEXT: State 37 (fall through)
State 37
  9: where «where_3» => 9
  10: true => 10
    TEST where «where_3»
    THEN: State 38 (fall through)
    ELSE: State 39
State 38
  9: true => 9
    MATCH 9 with value 9
    END
State 39
  10: true => 10
    MATCH 10 with value 10
    END
end # of state machine

"""
@testset "Additional tests to improve code coverage" begin
    io = IOBuffer()
    Rematch2.@match2_dumpall io e begin
        1                            => 1
        Foo(x, x)                    => 2
        y::D                       => 3
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
    # This will help figure out why:
        # println()
        # println(actual)
        # println()
        # for (a, e) in zip(split(actual, '\n'), split(expected, '\n'))
        #     @test a == e
        # end
    #
    # @test actual == expected
end

@testset "test @match2_dumpall for code coverage" begin
    # Since @match2_dumpall is only used for debugging, we don't need to test
    # its actual output.  Just make sure it doesn't crash.
    devnull = IOBuffer()
    Rematch2.@match2_dumpall devnull some_value begin
        Foo(x, 2) where !f1(x)            => 1
        Foo(1, y) where !f2(y)            => 2
        Foo(x, y) where !(f1(x) || f2(y)) => 3
        _                                 => 5
    end
    Rematch2.@match2_dump devnull some_value begin
        Foo(x, 2) where !f1(x)            => 1
        Foo(1, y) where !f2(y)            => 2
        Foo(x, y) where !(f1(x) || f2(y)) => 3
        _                                 => 5
    end
    @test true
end
