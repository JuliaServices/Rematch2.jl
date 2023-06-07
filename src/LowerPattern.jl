
function assignments(assigned::ImmutableDict{Symbol, Symbol})
    # produce a list of assignments to be splatted into the caller
    return (:($patvar = $resultsym) for (patvar, resultsym) in assigned)
end

# return the code needed for a pattern.
function code(bound_pattern::BoundPattern, ::BinderState)
    location = bound_pattern.location
    error("$(location.file):$(location.line): Internal error in Rematch2: `code(::$(typeof(bound_pattern)), ::$BinderState)` not implemented.")
end
code(bound_pattern::BoundTruePattern, state::BinderState) = true
code(bound_pattern::BoundFalsePattern, state::BinderState) = false
function code(bound_pattern::BoundEqualValueTestPattern, state::BinderState)
    :($isequal($(bound_pattern.input), $(bound_pattern.value)))
end
function code(bound_pattern::BoundRelationalTestPattern, state::BinderState)
    @assert bound_pattern.relation == :>=
    :($(bound_pattern.relation)($(bound_pattern.input), $(bound_pattern.value)))
end
function code(bound_pattern::BoundWhereTestPattern, state::BinderState)
    bound_pattern.inverted ? :(!$(bound_pattern.input)) : bound_pattern.input
end
function code(bound_pattern::BoundTypeTestPattern, state::BinderState)
    # We assert that the type is invariant.  Because this mutates state.assertions,
    # you must take the value of state.assertions after all calls to code.
    if bound_pattern.source != bound_pattern.type && !(bound_pattern.source in state.asserted_types)
        test = :($(bound_pattern.type) == $(bound_pattern.source))
        thrown = :($throw($AssertionError($string($(string(bound_pattern.location.file)),
            ":", $(bound_pattern.location.line),
            ": The type syntax `::", $(string(bound_pattern.source)), "` bound to type ",
            $string($(bound_pattern.type)), " at macro expansion time but ",
             $(bound_pattern.source), " later."))))
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
function code(bound_pattern::BoundFetchPattern, state::BinderState)
    tempvar = get_temp(state, bound_pattern)
    :($tempvar = $(code(bound_pattern)))
end

function code(bound_pattern::BoundFetchPattern)
    location = bound_pattern.location
    error("$(location.file):$(location.line): Internal error in Rematch2: `code(::$(typeof(bound_pattern)))` not implemented.")
end
function code(bound_pattern::BoundFetchFieldPattern)
    :($getfield($(bound_pattern.input), $(QuoteNode(bound_pattern.field_name))))
end
function code(bound_pattern::BoundFetchIndexPattern)
    i = bound_pattern.index
    if i < 0
        i = :($length($(bound_pattern.input)) + $(i + 1))
    end
    :($getindex($(bound_pattern.input), $i))
end
function code(bound_pattern::BoundFetchRangePattern)
    index = :($(bound_pattern.first_index):(length($(bound_pattern.input)) - $(bound_pattern.from_end)))
    :($getindex($(bound_pattern.input), $(index)))
end
function code(bound_pattern::BoundFetchLengthPattern)
    :($length($(bound_pattern.input)))
end
function code(bound_pattern::BoundFetchExpressionPattern)
    bound_pattern.value
end

# Return an expression that computes whether or not the pattern matches.
function lower_pattern_to_boolean(bound_pattern::BoundPattern, state::BinderState)
    Expr(:block, bound_pattern.location, code(bound_pattern, state))
end
function lower_pattern_to_boolean(bound_pattern::BoundFetchPattern, state::BinderState)
    # since fetches are performed purely for their side-effects, and
    # joined to the computations that require the fetched value using `and`,
    # we return `true` as the boolean value whenever we perform one.
    # (Fetches always succeed)
    Expr(:block, bound_pattern.location, code(bound_pattern, state), true)
end
