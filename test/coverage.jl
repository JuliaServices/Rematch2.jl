#
# Additional tests to improve node coverage.
# These tests mainly exercise diagnostic node that isn't crucial to the
# normal operation of the package.
#

abstract type A; end
struct B <: A; end
struct C <: A; end
struct D; x; end

struct Trash <: Rematch2.BoundFetchPattern
    location::LineNumberNode
end

expected="""

Decision Automaton: (57 nodes) input «input_value»
State 1
  1: «input_value» == 1 => 1
  2: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value».y && «input_value.y» == «input_value.x») => 2
  3: «input_value» isa Main.Rematch2Tests.D => 3
  4: («input_value» isa AbstractArray && length(«input_value») && «length(input_value)» >= 2 && «input_value»[2:(length(«input_value»)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && length(«input_value») && «length(input_value)» >= 2 && «input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
  10: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value».y && «input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value.x» == 1 && «input_value».y && (f2)(«input_value.y») && «where_5») => 2
  12: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value».y && (f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «input_value» == 1
    THEN: State 2 (fall through)
    ELSE: State 3
State 2
  1: true => 1
    MATCH 1 with value 1
State 3
  2: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value».y && «input_value.y» == «input_value.x») => 2
  3: «input_value» isa Main.Rematch2Tests.D => 3
  4: («input_value» isa AbstractArray && length(«input_value») && «length(input_value)» >= 2 && «input_value»[2:(length(«input_value»)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && length(«input_value») && «length(input_value)» >= 2 && «input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
  10: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value».y && «input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value.x» == 1 && «input_value».y && (f2)(«input_value.y») && «where_5») => 2
  12: («input_value» isa Main.Rematch2Tests.Foo && «input_value».x && «input_value».y && (f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «input_value» isa Main.Rematch2Tests.Foo
    THEN: State 4 (fall through)
    ELSE: State 26
State 4
  2: («input_value».x && «input_value».y && «input_value.y» == «input_value.x») => 2
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: («input_value».x && «input_value».y && «input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value».x && «input_value.x» == 1 && «input_value».y && (f2)(«input_value.y») && «where_5») => 2
  12: («input_value».x && «input_value».y && (f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    FETCH «input_value.x» := «input_value».x
    NEXT: State 5 (fall through)
State 5
  2: («input_value».y && «input_value.y» == «input_value.x») => 2
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: («input_value».y && «input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value.x» == 1 && «input_value».y && (f2)(«input_value.y») && «where_5») => 2
  12: («input_value».y && (f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    FETCH «input_value.y» := «input_value».y
    NEXT: State 6 (fall through)
State 6
  2: «input_value.y» == «input_value.x» => 2
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: («input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «input_value.y» == «input_value.x»
    THEN: State 7 (fall through)
    ELSE: State 8
State 7
  2: true => 2
    MATCH 2 with value 2
State 8
  6: («input_value» == 6 || «input_value» == 7) => 6
  10: («input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «input_value» == 6
    THEN: State 44
    ELSE: State 9
State 9
  6: «input_value» == 7 => 6
  10: («input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «input_value» == 7
    THEN: State 44
    ELSE: State 10
State 10
  10: («input_value.y» == 2 && (f1)(«input_value.x») && «where_4») => 1
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «input_value.y» == 2
    THEN: State 11 (fall through)
    ELSE: State 17
State 11
  10: ((f1)(«input_value.x») && «where_4») => 1
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    FETCH «where_4» := (f1)(«input_value.x»)
    NEXT: State 12 (fall through)
State 12
  10: «where_4» => 1
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
  12: («where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «where_4»
    THEN: State 13 (fall through)
    ELSE: State 14
State 13
  10: true => 1
    MATCH 10 with value 1
State 14
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
    TEST «input_value.x» == 1
    THEN: State 15 (fall through)
    ELSE: State 57
State 15
  11: ((f2)(«input_value.y») && «where_5») => 2
    FETCH «where_5» := (f2)(«input_value.y»)
    NEXT: State 16 (fall through)
State 16
  11: «where_5» => 2
    TEST «where_5»
    THEN: State 20
    ELSE: State 57
State 17
  11: («input_value.x» == 1 && (f2)(«input_value.y») && «where_5») => 2
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «input_value.x» == 1
    THEN: State 18 (fall through)
    ELSE: State 21
State 18
  11: ((f2)(«input_value.y») && «where_5») => 2
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    FETCH «where_5» := (f2)(«input_value.y»)
    NEXT: State 19 (fall through)
State 19
  11: «where_5» => 2
  12: ((f1)(«input_value.x») && «where_4» && «where_5») => 3
    TEST «where_5»
    THEN: State 20 (fall through)
    ELSE: State 57
State 20
  11: true => 2
    MATCH 11 with value 2
State 21
  12: ((f1)(«input_value.x») && «where_4» && (f2)(«input_value.y») && «where_5») => 3
    FETCH «where_4» := (f1)(«input_value.x»)
    NEXT: State 22 (fall through)
State 22
  12: («where_4» && (f2)(«input_value.y») && «where_5») => 3
    TEST «where_4»
    THEN: State 23 (fall through)
    ELSE: State 57
State 23
  12: ((f2)(«input_value.y») && «where_5») => 3
    FETCH «where_5» := (f2)(«input_value.y»)
    NEXT: State 24 (fall through)
State 24
  12: «where_5» => 3
    TEST «where_5»
    THEN: State 25 (fall through)
    ELSE: State 57
State 25
  12: true => 3
    MATCH 12 with value 3
State 26
  3: «input_value» isa Main.Rematch2Tests.D => 3
  4: («input_value» isa AbstractArray && length(«input_value») && «length(input_value)» >= 2 && «input_value»[2:(length(«input_value»)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && length(«input_value») && «length(input_value)» >= 2 && «input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» isa Main.Rematch2Tests.D
    THEN: State 27 (fall through)
    ELSE: State 28
State 27
  3: true => 3
    MATCH 3 with value 3
State 28
  4: («input_value» isa AbstractArray && length(«input_value») && «length(input_value)» >= 2 && «input_value»[2:(length(«input_value»)-1)]) => «input_value[2:(length-1)]»
  5: («input_value» isa Tuple && length(«input_value») && «length(input_value)» >= 2 && «input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» isa AbstractArray
    THEN: State 29 (fall through)
    ELSE: State 33
State 29
  4: (length(«input_value») && «length(input_value)» >= 2 && «input_value»[2:(length(«input_value»)-1)]) => «input_value[2:(length-1)]»
  6: («input_value» == 6 || «input_value» == 7) => 6
    FETCH «length(input_value)» := length(«input_value»)
    NEXT: State 30 (fall through)
State 30
  4: («length(input_value)» >= 2 && «input_value»[2:(length(«input_value»)-1)]) => «input_value[2:(length-1)]»
  6: («input_value» == 6 || «input_value» == 7) => 6
    TEST «length(input_value)» >= 2
    THEN: State 31 (fall through)
    ELSE: State 40
State 31
  4: «input_value[2:(length-1)]» := «input_value»[2:(length(«input_value»)-1)] => «input_value[2:(length-1)]»
    FETCH «input_value[2:(length-1)]» := «input_value»[2:(length(«input_value»)-1)]
    NEXT: State 32 (fall through)
State 32
  4: true => «input_value[2:(length-1)]»
    MATCH 4 with value «input_value[2:(length-1)]»
State 33
  5: («input_value» isa Tuple && length(«input_value») && «length(input_value)» >= 2 && «input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» isa Tuple
    THEN: State 34 (fall through)
    ELSE: State 42
State 34
  5: (length(«input_value») && «length(input_value)» >= 2 && «input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
    FETCH «length(input_value)» := length(«input_value»)
    NEXT: State 35 (fall through)
State 35
  5: («length(input_value)» >= 2 && «input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
    TEST «length(input_value)» >= 2
    THEN: State 36 (fall through)
    ELSE: State 40
State 36
  5: («input_value»[-1] && (e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
    FETCH «input_value[-1]» := «input_value»[-1]
    NEXT: State 37 (fall through)
State 37
  5: ((e).q1 && «where_0») => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
    FETCH «where_0» := (e).q1
    NEXT: State 38 (fall through)
State 38
  5: «where_0» => «input_value[-1]»
  6: («input_value» == 6 || «input_value» == 7) => 6
    TEST «where_0»
    THEN: State 39 (fall through)
    ELSE: State 40
State 39
  5: true => «input_value[-1]»
    MATCH 5 with value «input_value[-1]»
State 40
  6: («input_value» == 6 || «input_value» == 7) => 6
    TEST «input_value» == 6
    THEN: State 44
    ELSE: State 41
State 41
  6: «input_value» == 7 => 6
    TEST «input_value» == 7
    THEN: State 44
    ELSE: State 57
State 42
  6: («input_value» == 6 || «input_value» == 7) => 6
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» == 6
    THEN: State 44
    ELSE: State 43
State 43
  6: «input_value» == 7 => 6
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» == 7
    THEN: State 44 (fall through)
    ELSE: State 45
State 44
  6: true => 6
    MATCH 6 with value 6
State 45
  7: («input_value» isa Main.Rematch2Tests.A && (e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» isa Main.Rematch2Tests.A
    THEN: State 46 (fall through)
    ELSE: State 57
State 46
  7: ((e).q2 && «where_1») => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    FETCH «where_1» := (e).q2
    NEXT: State 47 (fall through)
State 47
  7: «where_1» => 7
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «where_1»
    THEN: State 48 (fall through)
    ELSE: State 49
State 48
  7: true => 7
    MATCH 7 with value 7
State 49
  8: («input_value» isa Main.Rematch2Tests.B && (e).q3 && «where_2») => 8
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» isa Main.Rematch2Tests.B
    THEN: State 50 (fall through)
    ELSE: State 53
State 50
  8: ((e).q3 && «where_2») => 8
    FETCH «where_2» := (e).q3
    NEXT: State 51 (fall through)
State 51
  8: «where_2» => 8
    TEST «where_2»
    THEN: State 52 (fall through)
    ELSE: State 57
State 52
  8: true => 8
    MATCH 8 with value 8
State 53
  9: («input_value» isa Main.Rematch2Tests.C && (e).q4 && «where_3») => 9
    TEST «input_value» isa Main.Rematch2Tests.C
    THEN: State 54 (fall through)
    ELSE: State 57
State 54
  9: ((e).q4 && «where_3») => 9
    FETCH «where_3» := (e).q4
    NEXT: State 55 (fall through)
State 55
  9: «where_3» => 9
    TEST «where_3»
    THEN: State 56 (fall through)
    ELSE: State 57
State 56
  9: true => 9
    MATCH 9 with value 9
State 57
    FAIL (throw)((Rematch2.MatchFailure)(«input_value»))
end # of automaton
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
        Foo(x, 2) where f1(x)            => 1
        Foo(1, y) where f2(y)            => 2
        Foo(x, y) where (f1(x) && f2(y)) => 3
    end
    actual = String(take!(io))

    for (a, e) in zip(split(actual, '\n'), split(expected, '\n'))
        @test a == e
    end

    @test_throws ErrorException Rematch2.gentemp(:a)

    trash = Trash(LineNumberNode(@__LINE__, @__FILE__))
    @test_throws ErrorException Rematch2.code(trash)
end
