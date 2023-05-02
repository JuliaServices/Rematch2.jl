
function assignments(assigned::ImmutableDict{Symbol, Symbol})
    # produce a list of assignments to be splatted into the caller
    a = (:($(esc(patvar)) = $resultsym) for (patvar, resultsym) in assigned)
end

# return the code needed for a pattern.
function code(bound_pattern::BoundPattern, state::BinderState)
    location = bound_pattern.location
    error("$(location.file):$(location.line): `code` not implemented for `$(typeof(bound_pattern))`.")
end
code(bound_pattern::BoundTruePattern, state::BinderState) = true
code(bound_pattern::BoundFalsePattern, state::BinderState) = false
function code(bound_pattern::BoundEqualValueTestPattern, state::BinderState)
    value = bound_pattern.value
    needs_let = value isa Expr || !isempty(bound_pattern.assigned)
    eval = :(isequal($(bound_pattern.input), $(bound_pattern.value)))
    if needs_let
        block = Expr(:block, bound_pattern.location, eval)
        Expr(:let, Expr(:block, assignments(bound_pattern.assigned)...), block)
    else
        eval
    end
end
function code(bound_pattern::BoundRelationalTestPattern, state::BinderState)
    @assert bound_pattern.relation == :>=
    :($(bound_pattern.relation)($(bound_pattern.input), $(esc(bound_pattern.value))))
end
function code(bound_pattern::BoundWhereTestPattern, state::BinderState)
    eval = esc(bound_pattern.source)
    block = Expr(:block, bound_pattern.location, eval)
    Expr(:let, Expr(:block, assignments(bound_pattern.assigned)...), block)
end
function code(bound_pattern::BoundTypeTestPattern, state::BinderState)
    # We assert that the type is invariant.  Because this mutates state.assertions,
    # you must take the value of state.assertions after all calls to code.
    if bound_pattern.source != bound_pattern.type && !(bound_pattern.source in state.asserted_types)
        test = :($(esc(:($(bound_pattern.type) == $(bound_pattern.source)))))
        thrown = :(throw(AssertionError(string($(string(bound_pattern.location.file)),
            ":", $(bound_pattern.location.line),
            ": The type syntax `::", $(string(bound_pattern.source)), "` bound to type ",
            string($(bound_pattern.type)), " at macro expansion time but ",
             $(esc(bound_pattern.source)), " later."))))
        push!(state.assertions, Expr(:block, bound_pattern.location, :($test || $thrown)))
        push!(state.asserted_types, bound_pattern.source)
    end
    :($(bound_pattern.input) isa $(bound_pattern.type))
end
function code(bound_pattern::BoundOrPattern, state::BinderState)
    :($(mapreduce(bp -> lower_pattern_to_boolean(bp, state),
            (a, b) -> :($a || $b),
            bound_pattern.subpatterns)))
end
function code(bound_pattern::BoundAndPattern, state::BinderState)
    :($(mapreduce(bp -> lower_pattern_to_boolean(bp, state),
            (a, b) -> :($a && $b),
            bound_pattern.subpatterns)))
end
function code(bound_pattern::BoundFetchFieldPattern, state::BinderState)
    tempvar = state.assignments[bound_pattern]
    index = QuoteNode(bound_pattern.field_name)
    :($tempvar = getfield($(bound_pattern.input), $index))
end
function code(
    bound_pattern::BoundFetchIndexPattern,
    state::BinderState)
    tempvar = state.assignments[bound_pattern]
    i = bound_pattern.index
    if i < 0; i = :(length($(bound_pattern.input)) + $(i + 1)); end
    :($tempvar = getindex($(bound_pattern.input), $i))
end
function code(bound_pattern::BoundFetchRangePattern, state::BinderState)
    tempvar = state.assignments[bound_pattern]
    index = :($(bound_pattern.first_index):(length($(bound_pattern.input)) - $(bound_pattern.from_end)))
    :($(tempvar) = getindex($(bound_pattern.input), $(index)))
end
function code(bound_pattern::BoundFetchLengthPattern, state::BinderState)
    tempvar = state.assignments[bound_pattern]
    :($tempvar = length($(bound_pattern.input)))
end
function code(bound_pattern::BoundFetchBindingPattern, state::BinderState)
    tempvar = get_temp(bound_pattern.variable)
    :($(tempvar) = $(bound_pattern.input))
end

# Return an expression that computes whether or not the pattern matches.
function lower_pattern_to_boolean(bound_pattern::BoundPattern, state::BinderState)
    Expr(:block, bound_pattern.location, code(bound_pattern, state))
end
function lower_pattern_to_boolean(
    bound_pattern::Union{BoundAndPattern, BoundOrPattern, BoundEqualValueTestPattern},
    state::BinderState)
    code(bound_pattern, state)
end
function lower_pattern_to_boolean(bound_pattern::BoundFetchPattern, state::BinderState)
    # since fetches are performed purely for their side-effects, and
    # joined to the computations that require the fetched value using `and`,
    # we return `true` as the boolean value whenever we perform one.
    # (Fetches always succeed)
    Expr(:block, bound_pattern.location, code(bound_pattern, state), true)
end
