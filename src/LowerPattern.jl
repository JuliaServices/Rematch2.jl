
function assignments(assigned::ImmutableDict{Symbol, Symbol})
    # produce a list of assignments to be splatted into the caller
    return (:($patvar = $resultsym) for (patvar, resultsym) in assigned)
end

# return the code needed for a pattern.
code(bound_pattern::BoundTruePattern, binder::BinderContext) = true
function code(bound_pattern::BoundEqualValueTestPattern, binder::BinderContext)
    :($isequal($(bound_pattern.input), $(code(bound_pattern.bound_expression))))
end
function code(bound_pattern::BoundRelationalTestPattern, binder::BinderContext)
    @assert bound_pattern.relation == :>=
    :($(bound_pattern.relation)($(bound_pattern.input), $(bound_pattern.value)))
end
function code(bound_pattern::BoundWhereTestPattern, binder::BinderContext)
    bound_pattern.inverted ? :(!$(bound_pattern.input)) : bound_pattern.input
end
function code(bound_pattern::BoundTypeTestPattern, binder::BinderContext)
    # We assert that the type is invariant.  Because this mutates binder.assertions,
    # you must take the value of binder.assertions after all calls to the generated code.
    if bound_pattern.source != bound_pattern.type && !(bound_pattern.source in binder.asserted_types)
        test = :($(bound_pattern.type) == $(bound_pattern.source))
        thrown = :($throw($AssertionError($string($(string(bound_pattern.location.file)),
            ":", $(bound_pattern.location.line),
            ": The type syntax `::", $(string(bound_pattern.source)), "` bound to type ",
            $string($(bound_pattern.type)), " at macro expansion time but ",
             $(bound_pattern.source), " later."))))
        push!(binder.assertions, Expr(:block, bound_pattern.location, :($test || $thrown)))
        push!(binder.asserted_types, bound_pattern.source)
    end
    :($(bound_pattern.input) isa $(bound_pattern.type))
end
function code(bound_pattern::BoundOrPattern, binder::BinderContext)
    :($(mapreduce(bp -> lower_pattern_to_boolean(bp, binder),
            (a, b) -> :($a || $b),
            bound_pattern.subpatterns)))
end
function code(bound_pattern::BoundAndPattern, binder::BinderContext)
    :($(mapreduce(bp -> lower_pattern_to_boolean(bp, binder),
            (a, b) -> :($a && $b),
            bound_pattern.subpatterns)))
end
function code(bound_pattern::BoundFetchPattern, binder::BinderContext)
    tempvar = get_temp(binder, bound_pattern)
    :($tempvar = $(code(bound_pattern)))
end

function code(bound_pattern::BoundFetchPattern)
    location = loc(bound_pattern)
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
    code(bound_pattern.bound_expression)
end

# Return an expression that computes whether or not the pattern matches.
function lower_pattern_to_boolean(bound_pattern::BoundPattern, binder::BinderContext)
    Expr(:block, loc(bound_pattern), code(bound_pattern, binder))
end
function lower_pattern_to_boolean(bound_pattern::BoundFetchPattern, binder::BinderContext)
    # since fetches are performed purely for their side-effects, and
    # joined to the computations that require the fetched value using `and`,
    # we return `true` as the boolean value whenever we perform one.
    # (Fetches always succeed)
    Expr(:block, loc(bound_pattern), code(bound_pattern, binder), true)
end
