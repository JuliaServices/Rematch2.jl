# Note we do not use `@eval` to define types within a `@testset``
# because we need the types to be defined during macro expansion,
# which is earlier than evaluation.  types are looked up during
# expansion of the `@match2` macro so we can use the known bindings
# of types to generate more efficient code.

file = Symbol(@__FILE__)

@enum Color Yellow Green Blue

macro casearm1(pattern, value)
    esc(:($pattern => $value))
end

macro casearm2(pattern, value)
    esc(:(@casearm1 $pattern $value))
end

macro check_is_identifier(x)
    false
end
macro check_is_identifier(x::Symbol)
    true
end

@testset "@rematch2 tests" begin

@testset "Check that `,if condition end` guards are parsed properly" begin
    x = true
    @test (@match2 3 begin
        ::Int, if x end => 1
        _ => 2
    end) == 1

    x = false
    @test (@match2 3 begin
        ::Int, if x end => 1
        _ => 2
    end) == 2
end

@testset "Check that `where` clauses are reparsed properly 1" begin
    x = true
    @test (@match2 3 begin
        ::Int where x => 1
        _ => 2
    end) == 1

    x = false
    @test (@match2 3 begin
        ::Int where x => 1
        _ => 2
    end) == 2
end

@testset "Check that `where` clauses are reparsed properly 2" begin
    x = true
    @test (@match2 3 begin
        a::Int where x => a
        _ => 2
    end) == 3

    x = false
    @test (@match2 3 begin
        a::Int where x => a
        _ => 2
    end) == 2
end

@testset "Check that `where` clauses are reparsed properly 3" begin
    let line = 0
        try
            line = (@__LINE__) + 2
            @eval @match2 Foo(1, 2) begin
                (Foo where unbound)(1, 2) => 1
            end
            @test false
        catch ex
            @test ex isa LoadError
            e = ex.error
            @test e isa ErrorException
            @test e.msg == "$file:$line: Unregognized pattern syntax `(Foo where unbound)(1, 2)`."
        end
    end
end

@testset "Check that `where` clauses are reparsed properly 4" begin
    for b1 in [false, true]
        for b2 in [false, true]
            @test (@match2 3 begin
                ::Int where b1 where b2 => 1
                _ => 2
            end) == ((b1 && b2) ? 1 : 2)
        end
    end
end

@testset "Check that `where` clauses are reparsed properly 5" begin
    for b1 in [false, true]
        for b2 in [false, true]
            @test (@match2 3 begin
                ::Int where b1 == b2 => 1
                _ => 2
            end) == ((b1 == b2) ? 1 : 2)
        end
    end
end

@testset "Assignments in the value do not leak out" begin
    @match2 Foo(1, 2) begin
        Foo(x, 2) => begin
            new_variable = 3
        end
    end
    @test !(@isdefined x)
    @test !(@isdefined new_variable)
end

@testset "Assignments in a where clause do not leak out" begin
    @match2 Foo(1, 2) begin
        Foo(x, 2) where begin
            new_variable = 3
            true
        end => begin
            @test !(@isdefined new_variable)
        end
    end
    @test !(@isdefined x)
    @test !(@isdefined new_variable)
end

@testset "A pure type pattern" begin
    @test (@match2 ::Symbol = :test1) == :test1
    @test (@match2 ::String = "test2") == "test2"
    @test_throws MatchFailure(:test1) @match2 ::String = :test1
    @test_throws MatchFailure("test2") @match2 ::Symbol = "test2"
end

@testset "bound variables may be used in subsequent interpolations" begin
    let x = nothing, y = nothing
        @test (@match2 (x, y, $(x + 2)) = (1, 2, 3)) == (1, 2, 3)
        @test x == 1
        @test y == 2
    end
end

#
# To print the decision automaton shown in comments below, replace @match2_count_nodes
# with @match2_dump and run the test.  To show the full details of how the decision
# automaton was computed, try @match2_dumpall.
#

@testset "test for decision automaton optimizations 1" begin
    # Node 1 TEST «input_value» isa Foo ELSE: Node 5 («label_0»)
    # Node 2 FETCH «input_value.y» := «input_value».y
    # Node 3 TEST «input_value.y» == 2 ELSE: Node 5 («label_0»)
    # Node 4 MATCH 1 with value 1
    # Node 5 («label_0») FAIL (throw)((Rematch2.MatchFailure)(«input_value»))
    @test (Rematch2.@match2_count_nodes some_value begin
        Foo(x, 2) => 1
    end) == 5
end

@testset "test for decision automaton optimizations 2" begin
    # Node 1 TEST «input_value» isa Foo ELSE: Node 6 («label_0»)
    # Node 2 FETCH «input_value.y» := «input_value».y
    # Node 3 TEST «input_value.y» == 2 ELSE: Node 5 («label_1»)
    # Node 4 MATCH 1 with value 1
    # Node 5 («label_1») MATCH 2 with value 2
    # Node 6 («label_0») MATCH 3 with value 4
    @test (Rematch2.@match2_count_nodes some_value begin
        Foo(x, 2) => 1
        Foo(_, _) => 2
        _ => 4
    end) == 6
end

@testset "test for decision automaton optimizations 3" begin
    # Node 1 TEST «input_value» isa Foo ELSE: Node 7 («label_0»)
    # Node 2 FETCH «input_value.x» := «input_value».x
    # Node 3 FETCH «input_value.y» := «input_value».y
    # Node 4 TEST «input_value.y» == 2 ELSE: Node 6 («label_1»)
    # Node 5 MATCH 1 with value (identity)(«input_value.x»)
    # Node 6 («label_1») MATCH 2 with value 2
    # Node 7 («label_0») FAIL (throw)((Rematch2.MatchFailure)(«input_value»))
    @test (Rematch2.@match2_count_nodes some_value begin
        Foo(x, 2) => x
        Foo(_, _) => 2
        _ => 4
    end) == 7
end

@testset "test for decision automaton optimizations 4" begin
    # Node 1 TEST «input_value» isa Foo ELSE: Node 7 («label_0»)
    # Node 2 FETCH «input_value.x» := «input_value».x
    # Node 3 FETCH «input_value.y» := «input_value».y
    # Node 4 TEST «input_value.y» == «input_value.x» ELSE: Node 6 («label_1»)
    # Node 5 MATCH 1 with value (identity)(«input_value.x»)
    # Node 6 («label_1») MATCH 2 with value 2
    # Node 7 («label_0») MATCH 3 with value 4
    @test (Rematch2.@match2_count_nodes some_value begin
        Foo(x, x) => x
        Foo(_, _) => 2
        _ => 4
    end) == 7
end

@testset "test for sharing where clause conjuncts" begin
    # Node 1 TEST «input_value» isa Main.Rematch2Tests.Foo ELSE: Node 18 («label_2»)
    # Node 2 FETCH «input_value.x» := «input_value».x
    # Node 3 FETCH «input_value.y» := «input_value».y
    # Node 4 TEST «input_value.y» == 2 ELSE: Node 9 («label_5»)
    # Node 5 FETCH «where_0» := (f1)(«input_value.x»)
    # Node 6 TEST where «where_0» ELSE: Node 8 («label_4»)
    # Node 7 MATCH 1 with value 1
    # Node 8 («label_4») TEST «input_value.x» == 1 THEN: Node 10 ELSE: Node 18 («label_2»)
    # Node 9 («label_5») TEST «input_value.x» == 1 ELSE: Node 13 («label_3»)
    # Node 10 FETCH «where_1» := (f2)(«input_value.y»)
    # Node 11 TEST where «where_1» ELSE: Node 18 («label_2»)
    # Node 12 MATCH 2 with value 2
    # Node 13 («label_3») FETCH «where_0» := (f1)(«input_value.x»)
    # Node 14 TEST where «where_0» ELSE: Node 18 («label_2»)
    # Node 15 FETCH «where_1» := (f2)(«input_value.y»)
    # Node 16 TEST where «where_1» ELSE: Node 18 («label_2»)
    # Node 17 MATCH 3 with value 3
    # Node 18 («label_2») MATCH 4 with value 4
    @test (Rematch2.@match2_count_nodes some_value begin
        Foo(x, 2) where f1(x)            => 1
        Foo(1, y) where f2(y)            => 2
        Foo(x, y) where (f1(x) && f2(y)) => 3
        _                                => 4
    end) == 18
end

@testset "test for sharing where clause disjuncts" begin
    # Node 1 TEST «input_value» isa Main.Rematch2Tests.Foo ELSE: Node 18 («label_2»)
    # Node 2 FETCH «input_value.x» := «input_value».x
    # Node 3 FETCH «input_value.y» := «input_value».y
    # Node 4 TEST «input_value.y» == 2 ELSE: Node 11 («label_3»)
    # Node 5 FETCH «where_0» := (f1)((identity)(«input_value.x»))
    # Node 6 TEST !«where_0» ELSE: Node 8 («label_5»)
    # Node 7 MATCH 1 with value 1
    # Node 8 («label_4») TEST «input_value.x» == 1 THEN: Node 10 ELSE: Node 18 («label_2»)
    # Node 9 («label_5») TEST «input_value.x» == 1 ELSE: Node 13 («label_3»)
    # Node 10 FETCH «where_1» := (f2)(«input_value.y»)
    # Node 11 TEST where !«where_1» ELSE: Node 18 («label_2»)
    # Node 12 MATCH 2 with value 2
    # Node 13 («label_3») FETCH «where_0» := (f1)(«input_value.x»)
    # Node 14 TEST where !«where_0» ELSE: Node 18 («label_2»)
    # Node 15 FETCH «where_1» := (f2)(«input_value.y»)
    # Node 16 TEST where !«where_1» ELSE: Node 18 («label_2»)
    # Node 17 MATCH 3 with value 3
    # Node 18 («label_2») MATCH 4 with value 5
    @test (Rematch2.@match2_count_nodes some_value begin
        Foo(x, 2) where !f1(x)            => 1
        Foo(1, y) where !f2(y)            => 2
        Foo(x, y) where !(f1(x) || f2(y)) => 3
        _                                 => 5
    end) == 18
end

@testset "exercise the dumping code for coverage" begin
    io = IOBuffer()
    @test (Rematch2.@match2_dumpall io some_value begin
        Foo(x, 2) where !f1(x)            => 1
        Foo(1, y) where !f2(y)            => 2
        Foo(x, y) where !(f1(x) || f2(y)) => 3
        _                                 => 5
    end) == 18
    @test (Rematch2.@match2_dump io some_value begin
        Foo(x, 2) where !f1(x)            => 1
        Foo(1, y) where !f2(y)            => 2
        Foo(x, y) where !(f1(x) || f2(y)) => 3
        _                                 => 5
    end) == 18
end

@testset "test for correct semantics of complex where clauses" begin
    function f1(a, b, c, d, e, f, g, h)
        @match2 (a, b, c, d, e, f, g, h) begin
            (a, b, c, d, e, f, g, h) where (!(!((!a || !b) && (c || !d)) || !(!e || f) && (g || h))) => 1
            (a, b, c, d, e, f, g, h) where (!((!a || b) && (c || d) || (e || !f) && (!g || !h))) => 2
            (a, b, c, d, e, f, g, h) where (!((a || b) && !(!c || !d) || !(!(!e || f) && !(g || !h)))) => 3
            (a, b, c, d, e, f, g, h) where (!(!(a || !b) && (!c || !d)) || !(!(e || !f) && (!g || h))) => 4
            (a, b, c, d, e, f, g, h) where (!(a || !b) && (!c || d) || (e || f) && !(!g || h)) => 5
            _ => 6
        end
    end
    function f2(a, b, c, d, e, f, g, h)
        # For reference we use the brute-force implementation of pattern-matching that just
        # performs the tests sequentially, like writing an if-elseif-else chain.
        Rematch2.@match (a, b, c, d, e, f, g, h) begin
            (a, b, c, d, e, f, g, h) where (!(!((!a || !b) && (c || !d)) || !(!e || f) && (g || h))) => 1
            (a, b, c, d, e, f, g, h) where (!((!a || b) && (c || d) || (e || !f) && (!g || !h))) => 2
            (a, b, c, d, e, f, g, h) where (!((a || b) && !(!c || !d) || !(!(!e || f) && !(g || !h)))) => 3
            (a, b, c, d, e, f, g, h) where (!(!(a || !b) && (!c || !d)) || !(!(e || !f) && (!g || h))) => 4
            (a, b, c, d, e, f, g, h) where (!(a || !b) && (!c || d) || (e || f) && !(!g || h)) => 5
            _ => 6
        end
    end
    function f3(a, b, c, d, e, f, g, h)
        @test f1(a, b, c, d, e, f, g, h) == f2(a, b, c, d, e, f, g, h)
    end
    for t in Iterators.product(([false, true] for a in 1:8)...,)
        f3(t...)
    end
end

@testset "infer positional parameters from Rematch2.fieldnames(T) 1" begin
    # struct T207a
    #     x; y; z
    #     T207a(x, y) = new(x, y, x)
    # end
    # Rematch2.fieldnames(::Type{T207a}) = (:x, :y)
    r = @match2 T207a(1, 2) begin
        T207a(x, y) => x
    end
    @test r == 1
    r = @match2 T207a(1, 2) begin
        T207a(x, y) => y
    end
    @test r == 2
end

@testset "infer positional parameters from Rematch2.fieldnames(T) 3" begin
    # struct T207c
    #     x; y; z
    # end
    # T207c(x, y) = T207c(x, y, x)
    # Rematch2.fieldnames(::Type{T207c}) = (:x, :y)
    r = @match2 T207c(1, 2) begin
        T207c(x, y) => x
    end
    @test r == 1
    r = @match2 T207c(1, 2) begin
        T207c(x, y) => y
    end
    @test r == 2
end

@testset "infer positional parameters from Rematch2.fieldnames(T) 4" begin
    # struct T207d
    #     x; z; y
    #     T207d(x, y) = new(x, 23, y)
    # end
    # Rematch2.fieldnames(::Type{T207d}) = (:x, :y)
    r = @match2 T207d(1, 2) begin
        T207d(x, y) => x
    end
    @test r == 1
    r = @match2 T207d(1, 2) begin
        T207d(x, y) => y
    end
    @test r == 2
end

@testset "diagnostics produced are excellent" begin

    @testset "infer positional parameters from Rematch2.fieldnames(T) 2" begin
        # struct T207b
        #     x; y; z
        #     T207b(x, y; z = x) = new(x, y, z)
        # end
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 T207b(1, 2) begin
                    T207b(x, y) => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: The type `$T207b` has 3 fields but the pattern expects 2 fields."
            end
        end
    end

    @testset "stack trace for MatchFailure" begin
        let line = 0
            try
                line = (@__LINE__) + 1
                @eval @match2 ::String = :test1
                @test false
            catch e
                @test e isa MatchFailure
                @test e.value == :test1
                top = @where_thrown
                @test top.file == file
                @test top.line == line
            end
        end
    end

    @testset "could not bind a type" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 Foo(1, 2) begin
                    ::Unknown => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Could not bind `Unknown` as a type (due to `UndefVarError(:Unknown)`)."
            end
        end
    end

    @testset "attempt to match non-type 1" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 Foo(1, 2) begin
                    ::1 => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Invalid type name: `1`."
            end
        end
    end

    @testset "attempt to match non-type 2" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 Foo(1, 2) begin
                    ::Base => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Attempted to match non-type `Base` as a type."
            end
        end
    end

    @testset "location of error for redundant field patterns 1" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 Foo(1, 2) begin
                    Foo(x = x1,x = x2) => (x1, x2)
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Pattern `Foo(x = x1, x = x2)` has duplicate named arguments [:x, :x]."
            end
        end
    end

    @testset "location of error for redundant field patterns 2" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 Foo(1, 2) begin
                    Foo(x = x1, x = x2) => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Pattern `Foo(x = x1, x = x2)` has duplicate named arguments [:x, :x]."
            end
        end
    end

    @testset "mix positional and named field patterns" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 Foo(1, 2) begin
                    Foo(x = x1, x2) => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Pattern `Foo(x = x1, x2)` mixes named and positional arguments."
            end
        end
    end

    @testset "wrong field count" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 Foo(1, 2) begin
                    Foo(x, y, z) => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: The type `$Foo` has 2 fields but the pattern expects 3 fields."
            end
        end
    end

    @testset "field not found" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match Foo(1, 2) begin
                    Foo(z = 1) => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Type `$Foo` has no field `z`."
            end
        end
    end

    @testset "multiple splats" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 [1, 2, 3] begin
                    [x..., y, z...] => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: More than one `...` in pattern `[x..., y, z...]`."
            end
        end
    end

    @testset "unrecognized pattern syntax" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 1 begin
                    (x + y) => 1
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Unregognized pattern syntax `x + y`."
            end
        end
    end

    @testset "type binding changed 1" begin
        let line = 0
            try
                local String = Int64
                line = (@__LINE__) + 2
                @match2 1 begin
                    ::String => 1
                end
                @test false
            catch e
                @test e isa AssertionError
                @test e.msg == "$file:$line: The type syntax `::String` bound to type String at macro expansion time but Int64 later."
            end
        end
    end

    @testset "type binding changed 2" begin
        let line = 0
            try
                line = (@__LINE__) + 3
                function f(x::String) where { String }
                    @match2 x begin
                        ::String => 1
                    end
                end
                f(Int64(1))
                @test false
            catch e
                @test e isa AssertionError
                @test e.msg == "$file:$line: The type syntax `::String` bound to type String at macro expansion time but Int64 later."
            end
        end
    end

    @testset "bad match block syntax" begin
        let line = 0
            try
                line = (@__LINE__) + 1
                @eval @match2 a (b + c)
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Unrecognized @match block syntax: `b + c`."
            end
        end
    end

    @testset "bad match case syntax" begin
        let line = 0
            try
                line = (@__LINE__) + 2
                @eval @match2 1 begin
                    (2 + 2) = 4
                end
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test startswith(e.msg, "$file:$line: Unrecognized @match case syntax: `2 + 2 =")
            end
        end
    end

    if VERSION >= v"1.8"
        @testset "warn for unreachable cases" begin
            let line = (@__LINE__) + 5
                @test_warn(
                    "$file:$line: Case 2: `Foo(1, 2) =>` is not reachable.",
                    @eval @match2 Foo(1, 2) begin
                        Foo(_, _) => 1
                        Foo(1, 2) => 2
                    end
                    )
            end
        end
    end

    @testset "assignment to pattern variables are permitted but act locally" begin
        @test (@match2 1 begin
            x where begin
                @test x == 1
                x = 12
                @test x == 12
                true
            end => begin
                @test x == 1
                x = 13
                @test x == 13
                6
            end
        end) == 6
    end

    if VERSION >= v"1.8"
        @testset "type constraints on the input are observed" begin
            let line = (@__LINE__) + 7
                @test_warn(
                    "$file:$line: Case 4: `_ =>` is not reachable.",
                    @eval @match2 identity(BoolPair(true, false))::BoolPair begin
                        BoolPair(true, _)       => 1
                        BoolPair(_, true)       => 2
                        BoolPair(false, false)  => 3
                        _                       => 4 # unreachable
                    end
                    )
            end
        end
    end

    @testset "splatting interpolation is not supported" begin
        let line = 0
            try
                line = (@__LINE__) + 4
                Base.eval(@__MODULE__, @no_escape_quote begin
                    interp_values = [1, 2]
                    f(a) = @match2 a begin
                        [0, $(interp_values...), 3] => 1
                    end
                end)
                @test false
            catch ex
                @test ex isa LoadError
                e = ex.error
                @test e isa ErrorException
                @test e.msg == "$file:$line: Splatting not supported in interpolation: `interp_values...`."
            end
        end
    end

    @testset "pattern variables are simple identifiers in a closed scope" begin
        @match2 collect(1:5) begin
            [x, y..., z] => begin
                @test @check_is_identifier(x)
                @test @check_is_identifier(y)
                @test @check_is_identifier(z)
                # test that pattern variable names are preserved in the code
                @test string(:(x + z)) == "x + z"
                @test x == 1
                @test y == [2, 3, 4]
                @test z == 5
                x = 3
                @test x == 3
                q = 12
            end
        end
        @test !(@isdefined x)
        @test !(@isdefined y)
        @test !(@isdefined z)
        @test !(@isdefined q)
    end

    @testset "pattern variable names can be shadowed" begin
        @match2 collect(1:5) begin
            [x, y..., z] => begin
                f(x) = x + 1
                @test f(x) == 2
                @test f(z) == 6
                @test x == 1
            end
        end
        @test !(@isdefined x)
    end

    @testset "pattern variable names can be assigned (locally)" begin
        z = "something"
        q = "other"
        @test (@match2 collect(1:5) begin
            [x, y..., z] where begin
                @test x == 1
                @test z == 5
                x = 55
                y = 2
                z = 100
                @test x == 55
                q = "changed"
                true
            end=> begin
                @test x == 1
                @test z == 5
                @test @isdefined y
                x + z
            end
        end) == 6
        @test !(@isdefined x)
        @test !(@isdefined y)
        @test z == "something"
        @test q == "changed"
    end

end

@testset "ensure we use `isequal` and not `==`" begin
    function f(v)
        @match2 v begin
            0.0    => 1
            1.0    => 4
            -0.0   => 2
            _      => 3
        end
    end
    @test f(0.0) == 1
    @test f(1.0) == 4
    @test f(-0.0) == 2
    @test f(2.0) == 3
end

# Tests inherited from Rematch below

@testset "Match Struct by field names" begin
    # match one struct field by name
    let x = nothing
        x1 = nothing
        @test (@match2 Foo(1,2) begin
               Foo(x=x1) => x1
        end) == 1
        @test x == nothing
        @test x1 == nothing
    end

    # match struct with mix of by-value and by-field name
    let x1 = nothing
        @test (@match2 Foo(1,2) begin
               Foo(0,2) => nothing
               Foo(x=x1) => x1
        end) == 1
    end

    # match multiple struct fields by name
    let x1 = nothing, y1 = nothing
        @test (@match2 Foo(1,2) begin
               Foo(x=x1,y=y1) => (x1,y1)
        end) == (1,2)
    end

    # match struct field by name redundantly
    let x1 = nothing, x2 = nothing
        @test_throws LoadError (@eval @match2 Foo(1,2) begin
               Foo(x=x1,x=x2) => (x1,x2)
        end)
    end

    # variables in patterns are local, and can match multiple positions
    let z = 0
        @test z == 0
        @test (@match2 Foo(1,1) begin
               Foo(x=z, y=z) => z # inner z matches both x and y
               end) == 1
        @test z == 0 # no change to outer z
    end

    # variable in a pattern can match multiple positions
    @test_throws MatchFailure(Foo(1,2)) (@match2 Foo(1,2) begin
                                     Foo(x=x1, y=x1) => true
                                     end)
end

@testset "non-struct Matches" begin
    # throw MatchFailure if no matches
    @test_throws MatchFailure(:this) @match2 :this begin
        :that => :ok
    end

    # match against symbols
    @test (@match2 :this begin
        :this => :ok
    end) == :ok

    # treat macros as constants
    @test (@match2 v"1.2.0" begin
      v"1.2.0" => :ok
    end) == :ok

    # QuoteNodes
    @test (@match2 :(:x) begin
      :(:x) => :ok
    end) == :ok
    @test (@match2 :(:x+:y) begin
      :(:x + :y) => :ok
    end) == :ok
end

@testset "logical expressions with branches" begin
    # disjunction
    @test (@match2 (1,(2,3)) begin
      (1, (x,:nope) || (2,x)) => x
    end) == 3

    # disjunction and repeated variables
    @test (@match2 (1,(2,3), 3) begin
      (1, (x,:nope) || (2,x), x) => x
    end) == 3
    @test (@match2 (1,(2,3), 4) begin
      (1, (x,:nope) || (2,x), x) => x
      _ => :ok
    end) == :ok
    @test (@match2 (3,(2,3), 3) begin
      (x, (x,:nope) || (2,x), 3) => x
    end) == 3
    @test (@match2 (1,(2,3), 3) begin
      (x, (x,:nope) || (2,x), 3) => x
      _ => :ok
    end) == :ok
    @test (@match2 (3,(2,3), 3) begin
      (x, (x,:nope) || (2,x), x) => x
    end) == 3
    @test (@match2 (3,(2,3), 1) begin
      (x, (x,:nope) || (2,x), x) => x
      _ => :ok
    end) == :ok

    # conjunction
    @test (@match2 (1,(2,3)) begin
        (1, a && (2,b)) => (a,b)
    end) == ((2,3),3)
    @test_throws MatchFailure((1,(2,3))) (@match2 (1,(2,3)) begin
        (1, a && (1,b)) => (a,b)
    end) == ((2,3),3)

    # only vars that exist in all branches can be accessed
    @test_throws UndefVarError(:y) @match2 (1,(2,3)) begin
      (1, (x,:nope) || (2,y)) => y
    end
end

@testset "Splats" begin
    # splats
    test0(x) = @match2 x begin
        [a] => [a]
        [a,b,c...] => [a,b,c]
        (a,) => (a,)
        (a...,b,c,d) => (a,b,c,d)
        (a,b...,c) => (a,b,c)
        _ => false
    end
    @test test0([1]) == [1]
    @test test0([1,2]) == [1,2,[]]
    @test test0([1,2,3]) == [1,2,[3]]
    @test test0([1,2,3,4]) == [1,2,[3,4]]
    @test test0((1,)) == (1,)
    @test test0((1,2)) == (1, (), 2)
    @test test0((1,2,3)) == ((), 1, 2, 3)
    @test test0((1,2,3,4)) == ((1,), 2, 3, 4)
    @test test0((1,2,3,4,5)) == ((1,2), 3, 4, 5)

    # no splats allowed in structs (would be nice, but need to implement getfield(struct, range))
    @test_throws LoadError @eval @match2 foo begin
        Foo(x...) => :nope
    end

    # at most one splat in tuples/arrays
    @test_throws LoadError @eval @match2 [1,2,3] begin
        [a...,b,c...] => :nope
    end
    @test_throws LoadError @eval @match2 [1,2,3] begin
        (a...,b,c...) => :nope
    end

    # inference for splats
    infer1(x) = @match2 x begin
        (a, b..., c) => a
    end
    @test @inferred(infer1((:ok,2,3,4))) == :ok

    infer2(x) = @match2 x begin
        (a, b..., c) => c
    end

    @test @inferred(infer2((1,2,3,:ok))) == :ok
end

@testset "Inference in branches" begin
    # inference in branches
    infer3(foo) = @match2 foo begin
        Foo(_,y::Symbol) => y
        Foo(x::Symbol,_) => x
    end
    @test @inferred(infer3(Foo(1,:ok))) == :ok
    infer4(foo) = @match2 foo begin
        Foo(x,y::Symbol) => y
        Foo(x::Symbol,y) => x
    end
    @test @inferred(infer4(Foo(1,:ok))) == :ok
end

@testset "Nested Guards" begin
    # nested guards can use earlier bindings
    @test (@match2 [1,2] begin
      [x, y where y > x] => (x,y)
    end) == (1,2)
    @test_throws MatchFailure([2,1]) @match2 [2,1] begin
      [x, y where y > x] => (x,y)
    end

    # nested guards can't use later bindings
    @test_throws UndefVarError(:y) @match2 [2,1] begin
      [x where y > x, y ] => (x,y)
    end
end

@testset "structs matching all fields" begin
    # detect incorrect numbers of fields
    @test_throws LoadError (@eval @match2 Foo(x) = Foo(1,2)) == (1,2)
    @test_throws LoadError @eval @match2 Foo(x) = Foo(1,2)
    @test_throws LoadError @eval @match2 Foo(x,y,z) = Foo(1,2)

    # ...even if the pattern is not reached
    @test_throws LoadError (@eval @match2 Foo(1,2) begin
        Foo(x,y) => :ok
        Foo(x) => :nope
    end)
end

@testset "Miscellanea" begin
    # match against fiddly symbols (https://github.com/kmsquire/Match.jl/issues/32)
    @test (@match2 :(@when a < b) begin
            Expr(_, [Symbol("@when"), _, _]) => :ok
            Expr(_, [other, _, _]) => other
            end) == :ok

    # test repeated variables (https://github.com/kmsquire/Match.jl/issues/27)
    @test (@match2 (x,x) = (1,1)) == (1,1)
    @test_throws MatchFailure((1,2)) @match2 (x,x) = (1,2)

    # match against single tuples (https://github.com/kmsquire/Match.jl/issues/43)
    @test (@match2 (:x,) begin
      (:x,) => :ok
    end) == :ok

    # match against empty structs (https://github.com/kmsquire/Match.jl/issues/43)
    e = (True(), 1)
    @test (@match2 e begin
        (True(), x) => x
    end) == 1

    # symbols are not interpreted as variables (https://github.com/kmsquire/Match.jl/issues/45)
    let x = 42
        @test (@match2 (:x,) begin
          (:x,) => x
        end) == 42
    end

    # allow & and | for conjunction/disjunction (https://github.com/RelationalAI-oss/Rematch.jl/issues/1)
    @test (@match2 (1,(2,3)) begin
      (1, (x,:nope) | (2,x)) => x
    end) == 3
    @test (@match2 (1,(2,3)) begin
        (1, a & (2,b)) => (a,b)
    end) == ((2,3),3)

    @test_throws LoadError @eval @match2 a + b = x
end

@testset "Interpolated Values" begin
    # match against interpolated values
    let outer = 2, b = nothing, c = nothing
        @test (@match2 [1, $outer] = [1,2]) == [1,2]
        @test (@match2 (1, $outer, b..., c) = (1,2,3,4,5)) == (1,2,3,4,5)
        @test b == (3,4)
        @test c == 5
    end
    test_interp_pattern = let a=1, b=2, c=3,
                              arr=[10,20,30], tup=(100,200,300)
        _t(x) = @match2 x begin
            # scalars
            [$a,$b,$c,out] => out
            [fronts..., $a,$b,$c, back] => [fronts...,back]
            # arrays & tuples
            [fronts..., $arr, back] => [fronts...,back]
            [fronts..., $tup, back] => [fronts...,back]
            # complex expressions
            [$(a+b+c), out] => out
            # splatting existing values not supported
            # [fronts..., $(arr...), back] => [fronts...,back]
        end
    end
    # scalars
    @test test_interp_pattern([1,2,3,4]) == 4
    @test test_interp_pattern([4,3,2,1, 1,2,3, 4]) == [4,3,2,1,4]
    # arrays & tuples
    @test test_interp_pattern([0,1, [10,20,30], 2]) == [0,1,2]
    @test test_interp_pattern([0,1, (100,200,300), 2]) == [0,1,2]
    # complex expressions
    @test test_interp_pattern([6,1]) == 1
    # TODO: splatting existing values into pattern isn't suported
    # @test_broken test_interp_pattern([0,1, 10,20,30, 2]) == [0,1,2]
end

# # --- tests from Match.jl ---

@testset "Tests imported from Match.jl" begin

    # Type matching
    test1(item) = @match2 item begin
        n::Int                       => "Integers are awesome!"
        str::AbstractString          => "Strings are the best"
        m::Dict{Int,AbstractString}  => "Ints for Strings?"
        d::Dict                      => "A Dict! Looking up a word?"
        _                            => "Something unexpected"
    end

    d = Dict{Int,AbstractString}(1 => "a", 2 => "b")

    @test test1(66)     == "Integers are awesome!"
    @test test1("abc")  == "Strings are the best"
    @test test1(d)      == "Ints for Strings?"
    @test test1(Dict()) == "A Dict! Looking up a word?"
    @test test1(2.0)    == "Something unexpected"

    test2(person) = @match2 person begin
        Person("Julia", lastname,  _) => "Found Julia $lastname"
        Person(firstname, "Julia", _) => "$firstname Julia was here!"
        Person(firstname, lastname, Address(_, "Cambridge", zip)) => "$firstname $lastname lives in zip $zip"
        _::Person  => "Unknown person!"
    end

    @test test2(Person("Julia", "Robinson", Address("450 Serra Mall", "Stanford", "94305")))         == "Found Julia Robinson"
    @test test2(Person("Gaston", "Julia",   Address("1 rue Victor Cousin", "Paris", "75005")))       == "Gaston Julia was here!"
    @test test2(Person("Edwin", "Aldrin",   Address("350 Memorial Dr", "Cambridge", "02139")))       == "Edwin Aldrin lives in zip 02139"
    @test test2(Person("Linus", "Pauling",  Address("1200 E California Blvd", "Pasadena", "91125"))) == "Unknown person!"  # Really?

    function test_show(io::IO, term::Term)
        @match2 term begin
           Var(n)    => print(io, n)
           Fun(x, b) => begin
                            print(io, "^$x.")
                            test_show(io, b)
                        end
           App(f, v) => begin
                            print(io, "(")
                            test_show(io, f)
                            print(io, " ")
                            test_show(io, v)
                            print(io, ")")
                        end
        end
    end

    function is_identity_fun(term::Term)
       @match2 term begin
         Fun(x, Var(y)) where x == y => true
         _ => false
       end
    end

    id = Fun("x", Var("x"))
    t = Fun("x", Fun("y", App(Var("x"), Var("y"))))

    let io = IOBuffer()
        test_show(io, id)
        @test String(take!(io)) == "^x.x"
        test_show(io, t)
        @test String(take!(io)) == "^x.^y.(x y)"
        @test is_identity_fun(id)
        @test !is_identity_fun(t)
    end

    myisodd(x::Int) = @match2(x, i => i % 2 == 1)
    @test filter(myisodd, 1:10) == filter(isodd, 1:10) == [1, 3, 5, 7, 9]

    function parse_arg(arg::AbstractString, value::Any=nothing)
       @match2 (arg, value) begin
          ("-l",              lang) where lang != nothing => "Language set to $lang"
          ("-o" || "--optim", n::Int) where 0 < n <= 5 => "Optimization level set to $n"
          ("-o" || "--optim", n::Int)                         => "Illegal optimization level $(n)!"
          ("-h" || "--help",  nothing)                        => "Help!"
          bad                                                 => "Unknown argument: $bad"
       end
    end

    @test parse_arg("-l", "eng")  == "Language set to eng"
    @test parse_arg("-l")         == "Unknown argument: (\"-l\", nothing)"
    @test parse_arg("-o", 4)      == "Optimization level set to 4"
    @test parse_arg("--optim", 5) == "Optimization level set to 5"
    @test parse_arg("-o", 0)      == "Illegal optimization level 0!"
    @test parse_arg("-o", 1.0)    == "Unknown argument: (\"-o\", 1.0)"

    @test parse_arg("-h") == parse_arg("--help") == "Help!"

    function fizzbuzz(range::AbstractRange)
        io = IOBuffer()
        for n in range
            @match2 (n % 3, n % 5) begin
                (0, 0) => print(io, "fizzbuzz ")
                (0, _) => print(io, "fizz ")
                (_, 0) => print(io, "buzz ")
                (_, _) => print(io, n, ' ')
            end
        end
        String(take!(io))
    end

    @test fizzbuzz(1:15) == "1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz "

    function balance(tree::RBTree)
        @match2 tree begin
            (Black(z, Red(y, Red(x, a, b), c), d)
             || Black(z, Red(x, a, Red(y, b, c)), d)
             || Black(x, a, Red(z, Red(y, b, c), d))
             || Black(x, a, Red(y, b, Red(z, c, d)))) => Red(y, Black(x, a, b),
                                                             Black(z, c, d))
            _ => tree
        end
    end

    @test balance(Black(1, Red(2, Red(3, Leaf(), Leaf()), Leaf()), Leaf())) ==
                Red(2, Black(3, Leaf(), Leaf()),
                    Black(1, Leaf(), Leaf()))

    function num_match(n)
        @match2 n begin
            0      => "zero"
            1 || 2 => "one or two"
            # TODO: support range patterns
            # 3:10   => "three to ten"
            _      => "something else"
        end
    end

    @test num_match(0) == "zero"
    @test num_match(2) == "one or two"
    # @test num_match(4) == "three to ten"
    @test num_match(12) == "something else"
    @test num_match("hi") == "something else"
    @test num_match('c') == "something else"

    # Interpolation of matches in quoted expressions
    test_interp(item) = @match2 item begin
        [a, b] => :($a + $b)
    end
    @test test_interp([1, 2]) == :(1 + 2)
end

end
