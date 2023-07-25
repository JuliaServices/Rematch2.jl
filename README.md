# Rematch2 - Pattern-matching for Julia

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaServices.github.io/Rematch2.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://JuliaServices.github.io/Rematch2.jl/dev/)
[![Build Status](https://github.com/JuliaServices/Rematch2.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/JuliaServices/Rematch2.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/JuliaServices/Rematch2.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/JuliaServices/Rematch2.jl)

Pattern matching for Julia.  A rewrite of [`Rematch.jl`](https://github.com/RelationalAI-oss/Rematch.jl)
that offers improved performance at runtime, additional pattern forms, and improved control of
variable scopes.

`Rematch2.jl` provides a syntax sugar for matching Julia values against syntactic
patterns. The `@match` macro expands a pattern-matching syntax into a decision automaton
that performs a series of tests to determine the first pattern that matches the input.
These tests check the types and structure of the provided value according to the pattern,
allowing you to more simply write checks that describe your intent.

``` julia
julia> using Rematch2

julia> struct Foo
           x::Int64
           y::String
       end

julia> f(x) = @match x begin
           _::String => :string
           [a,a,a] => (:all_the_same, a)
           [a,bs...,c] => (:at_least_2, a, bs, c)
           Foo(x, "foo") where x > 1 => :foo
       end
f (generic function with 1 method)

julia> f("foo")
:string

julia> f([1,1,1])
(:all_the_same, 1)

julia> f([1,1])
(:at_least_2, 1, Int64[], 1)

julia> f([1,2,3,4])
(:at_least_2, 1, [2, 3], 4)

julia> f([1])
ERROR: MatchFailure([1])
Stacktrace:
 [1] f(x::Vector{Int64})
   @ Main ./REPL[6]:5
 [2] top-level scope
   @ REPL[11]:1

julia> f(Foo(2, "foo"))
:foo

julia> f(Foo(0, "foo"))
ERROR: MatchFailure(Foo(0, "foo"))
Stacktrace:
 [1] f(x::Foo)
   @ Main ./REPL[6]:5
 [2] top-level scope
   @ REPL[13]:1

julia> f(Foo(2, "not a foo"))
ERROR: MatchFailure(Foo(2, "not a foo"))
Stacktrace:
 [1] f(x::Foo)
   @ Main ./REPL[6]:5
 [2] top-level scope
   @ REPL[14]:1
```

## Usage

### Assignment Syntax
``` julia
@match pattern = value
```

If value matches pattern, binds variables and returns `value`. Otherwise, throws `MatchFailure`.

After evaluation, any variable names used within `pattern` will be bound as new variables in the enclosing scope. For example:
```julia
julia> @match Foo(x,2) = Foo(1,2)
Foo(1,2)

julia> x
1
```

### Case Syntax

``` julia
@match value begin
    pattern1 => result1
    pattern2 => result2
    ...
end
```

Returns `result` for the first matching pattern. If there are no matching patterns, throws `MatchFailure`.

Note that unlike the _assignment syntax_, this does not create any variable bindings outside the match macro.

## Patterns

* `_` matches anything
* `foo` matches anything, binds value to `foo`
* `Foo(x,y,z)` matches structs of type `Foo` with fields matching `x,y,z`
* `Foo(y=1)` matches structs of type `Foo` whose `y` field equals `1`
* `[x,y,z]` matches `AbstractArray`s with 3 entries matching `x,y,z`
* `(x,y,z)` matches `Tuple`s with 3 entries matching `x,y,z`
* `[x,y...,z]` matches `AbstractArray`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
* `(x,y...,z)` matches `Tuple`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
* `::T` matches any subtype (`isa`) of T
* `x::T` matches any subtype (`isa`) of T that also matches the pattern `x`
* `x || y` matches values which match either `x` or `y` (only variables which exist in both branches will be bound)
* `x && y` matches values which match both `x` and `y`
* `x where condition` matches only if `condition` is true (`condition` may use any variables that occur earlier in the pattern eg `(x, y, z where x + y > z)`)
* Anything else is treated as a constant and tested for equality
* Expressions can be interpolated in as constants via standard interpolation syntax `$(x)`.  Interpolations may use previously bound variables.

Patterns can be nested arbitrarily.

Repeated variables only match if they are equal (`==`). For example `(x,x)` matches `(1,1)` but not `(1,2)`.

## Early exit and failure

Inside the result part of a case, you can cause the pattern to fail (as if the pattern does not match), or you can return a value early:

```julia
@match value begin
    pattern1 => begin
        if some_failure_condition
            @match_fail
        end
        if some_shortcut_condition
            @match_return 1
        end
        ...
        2
    end
    ...
end
```

In this example, the result value when matching `pattern1` is a block that has two early exit conditions.
When `pattern1` matches but `some_failure_condition` is `true`, then the whole case is treated as not matching and the following cases are tried.
Otherwise, if `some_shortcut_condition` is `true`, then `1` is the result value for this case.
Otherwise `2` is the result.

## Differences from [Match.jl](https://github.com/kmsquire/Match.jl)

This package was branched from [Rematch.jl](https://github.com/RelationalAI-oss/Rematch.jl),
which was branched from the original [Match.jl](https://github.com/kmsquire/Match.jl).

### `Rematch` differs from `Match` in the following ways:

* If no branches are matched, throws `MatchFailure` instead of returning nothing.
* Matching against a struct with the wrong number of fields produces an error instead of silently failing.
* Repeated variables require equality, ie `@match (1,2) begin (x,x) => :ok end` fails.
* The syntax for guards is `x where x > 1` instead of `x, if x > 1 end` and can occur anywhere in a pattern.
* Structs can be matched by field-names, allowing partial matches: `@match Foo(1,2) begin Foo(y=2) => :ok end` returns `:ok`.
* Patterns support interpolation, ie `let x=1; @match ($x,$(x+1)) = (1,2); end` is a match.
* No support (yet) for matching `Regex` or `UnitRange`.
* No support (yet) for matching against multidimensional arrays - all array patterns use linear indexing.

### `Rematch2` differs from `Rematch` in the following ways:

* Errors always identify a specific line in the user's program where the problem occurred.
* Previously bound variables may now be used in interpolations, ie `@match (x, $(x+2)) = (1, 3)` is a match.
* A pure type match (without another pattern) can be written as `::Type`.
* Types appearing in type patterns (`::Type`) and struct patterns (`Type(...)`) are bound at macro-expansion time in the context of the module containing the macro usage.  As a consequence, you cannot use certain type expressions that would differ.  For example, you cannot use a type parameter or a local variable containing a type.  The generated code checks that the type is the same at evaluation time as it was at macro expansion time, and an error is thrown if they differ.  If this rare incompatibility affects you, you can use `x where x isa Type` as a workaround.  If the type is not defined at macro-expansion time, an error is issued.
* A warning is issued at macro-expansion time if a case cannot be reached because it is subsumed by prior cases.
