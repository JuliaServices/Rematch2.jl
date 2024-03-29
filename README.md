# This package has been archived.  Please use https://github.com/JuliaServices/Match.jl instead!

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaServices.github.io/Rematch2.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://JuliaServices.github.io/Rematch2.jl/dev/)
[![Build Status](https://github.com/JuliaServices/Rematch2.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/JuliaServices/Rematch2.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/JuliaServices/Rematch2.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/JuliaServices/Rematch2.jl)

# Advanced Pattern Matching for Julia

Features:

* Matching against almost any data type with a first-match policy
* Deep matching within data types and arrays
* Variable binding within matches
* Produces a decision automaton to avoid repeated tests between patterns.

## Installation
Use the Julia package manager.  Within Julia, do:
```julia
Pkg.add("Rematch2")
```

## Usage

The package provides two macros for pattern-matching: `@match` and `@ismatch`.
It is possible to supply variables inside patterns, which will be bound
to corresponding values.

    using Rematch2

    @match item begin
        pattern1              => result1
        pattern2, if cond end => result2
        pattern3 || pattern4  => result3
        _                     => default_result
    end

    if @ismatch value pattern
        # Code that uses variables bound in the pattern
    end

See the [documentation](https://JuliaServices.github.io/Rematch2.jl/stable/)
for examples of this and other features.

## Patterns

* `_` matches anything
* `x` (an identifier) matches anything, binds value to the variable `x`
* `T(x,y,z)` matches structs of type `T` with fields matching patterns `x,y,z`
* `T(y=1)` matches structs of type `T` whose `y` field equals `1`
* `[x,y,z]` matches `AbstractArray`s with 3 entries matching `x,y,z`
* `(x,y,z)` matches `Tuple`s with 3 entries matching `x,y,z`
* `[x,y...,z]` matches `AbstractArray`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
* `(x,y...,z)` matches `Tuple`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
* `::T` matches any subtype (`isa`) of type `T`
* `x::T` matches any subtype (`isa`) of T that also matches pattern `x`
* `x || y` matches values which match either pattern `x` or `y` (only variables which exist in both branches will be bound)
* `x && y` matches values which match both patterns `x` and `y`
* `x, if condition end` matches only if `condition` is true (`condition` may use any variables that occur earlier in the pattern eg `(x, y, z where x + y > z)`)
* `x where condition` An alternative form for `x, if condition end`
* Anything else is treated as a constant and tested for equality
* Expressions can be interpolated in as constants via standard interpolation syntax `\$(x)`.  Interpolations may use previously bound variables.

Patterns can be nested arbitrarily.

Repeated variables only match if they are equal (`isequal`). For example `(x,x)` matches `(1,1)` but not `(1,2)`.

## Early exit and failure

Inside the result part of a case, you can cause the pattern to fail (as if the pattern did not match), or you can return a value early:

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

## Differences from previous versions of `Match.jl`

* If no branches are matched, throws `MatchFailure` instead of returning nothing.
* Matching against a struct with the wrong number of fields produces an error instead of silently failing.
* Repeated variables require equality, ie `@match (1,2) begin (x,x) => :ok end` fails.
* We add a syntax for guards `x where x > 1` in addition to the existing `x, if x > 1 end`.
* Structs can be matched by field-names, allowing partial matches: `@match Foo(1,2) begin Foo(y=2) => :ok end` returns `:ok`.
* Patterns support interpolation, ie `let x=1; @match ($x,$(x+1)) = (1,2); end` is a match.
* We have dropped support for matching against multidimensional arrays - all array patterns use linear indexing.
* We no longer support the (undocumented) syntax `@match value pattern` which returned an array of the bindings of the pattern variables.
* Errors now identify a specific line in the user's program where the problem occurred.
* Previously bound variables may now be used in interpolations, ie `@match (x, $(x+2)) = (1, 3)` is a match.
* A pure type match (without another pattern) can be written as `::Type`.
* Types appearing in type patterns (`::Type`) and struct patterns (`Type(...)`) are bound at macro-expansion time in the context of the module containing the macro usage.  As a consequence, you cannot use certain type expressions that would differ.  For example, you cannot use a type parameter or a local variable containing a type.  The generated code checks that the type is the same at evaluation time as it was at macro expansion time, and an error is thrown if they differ.  If this rare incompatibility affects you, you can use `x where x isa Type` as a workaround.  If the type is not defined at macro-expansion time, an error is issued.
* A warning is issued at macro-expansion time if a case cannot be reached because it is subsumed by prior cases.
