function handle_match_eq(location::LineNumberNode, mod::Module, expr)
    @capture(expr, pattern_ = value_) ||
        return :(error($(string("Unrecognized @match syntax: ", expr))))

    input_variable::Symbol = gensym("input_value")
    (bound_pattern::BoundPattern, assigned::Dict{Symbol, Symbol}, state::BinderState) =
        bind_pattern(mod, location, pattern, input_variable)

    if !isempty(state.errors)
        errors = join(map(e -> e.second, state.errors), "\n")
        return Expr(:block, state.errors[1].first, :(error($errors)))
    end

    matched = lower_pattern(bound_pattern, state)
    Expr(:block,
        # evaluate the assertions
        state.assertions...,

        # compute the input into a variable so we do not repeat its side-effects
        :($input_variable = $(esc(value))),

        # check that it matched the pattern; if not throw an exception
        :($matched || throw(MatchFailure($input_variable))),

        # assign to pattern variables.
        (:($(esc(patvar)) = $resultsym) for (patvar, resultsym) in assigned)...,

        # finally, yield the input that was matched
        input_variable)
end

"""
Usage:

```
    @match pattern = value
```

If `value` matches `pattern`, bind variables and return `value`.
Otherwise, throw `MatchFailure`.
"""
macro match(expr)
    handle_match_eq(__source__, __module__, expr)
end

"""
Usage:
```
    @match value begin
        pattern1 => result1
        pattern2 => result2
        ...
    end
```

Return `result` for the first matching `pattern`.
If there are no matches, throw `MatchFailure`.
"""
macro match(value, cases)
    handle_match_cases(__source__, __module__, value, cases)
end

"""
This macro has two forms.

- The single-pattern form:

```
    @match pattern = value
```

  If `value` matches `pattern`, bind variables and return `value`.
  Otherwise, throw `MatchFailure`.

- The multi-pattern form:

```
    @match value begin
        pattern1 => result1
        pattern2 => result2
        ...
    end
```

  Return `result` for the first matching `pattern`.
  If there are no matches, throw `MatchFailure`.

Patterns:

  * `_` matches anything
  * `foo` matches anything, binds value to `foo`
  * `Foo(x,y,z)` matches structs of type `Foo` with fields matching `x,y,z`
  * `Foo(y=1)` matches structs of type `Foo` whose `y` field equals `1`
  * `[x,y,z]` matches `AbstractArray`s with 3 entries matching `x,y,z`
  * `(x,y,z)` matches `Tuple`s with 3 entries matching `x,y,z`
  * `[x,y...,z]` matches `AbstractArray`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
  * `(x,y...,z)` matches `Tuple`s with at least 2 entries, where `x` matches the first entry, `z` matches the last entry and `y` matches the remaining entries.
  * `_::T` matches any subtype (`isa`) of T
  * `x || y` matches values which match either `x` or `y` (only variables which exist in both branches will be bound)
  * `x && y` matches values which match both `x` and `y`
  * `x where condition` matches only if `condition` is true (`condition` may use any variables that occur earlier in the pattern eg `(x, y, z where x + y > z)`)
  * Anything else is treated as a constant and tested for equality
  * Expressions can be interpolated in as constants via standard interpolation syntax `\$(x)`

Patterns can be nested arbitrarily.

Repeated variables only match if they are equal (`==`). For example `(x,x)` matches `(1,1)` but not `(1,2)`.
"""
macro match end

export @match
