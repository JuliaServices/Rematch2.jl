
# Returns an expression that computes whether or not the pattern matches.
function lower_pattern_to_boolean(
    bound_pattern::BoundPattern,
    ::BinderState)
    error("lower_pattern_to_boolean not implemented for $(typeof(bound_pattern)))")
end

function lower_pattern_to_boolean(
    bound_pattern::TrueBoundPattern,
    ::BinderState)
    true
end

function lower_pattern_to_boolean(
    bound_pattern::FalseBoundPattern,
    ::BinderState)
    false
end

function lower_pattern_to_boolean(
    bound_pattern::EqualValueBoundPattern,
    ::BinderState)
    value = bound_pattern.value
    needs_let = value isa Expr || value isa Symbol && !(contains("#", string(value)))
    eval = :(isequal($(bound_pattern.input), $(bound_pattern.value)))
    if needs_let
        block = Expr(:block, bound_pattern.location, eval)
        Expr(:let, Expr(:block, assignments(bound_pattern.assigned)...), block)
    else
        eval
    end
end

function lower_pattern_to_boolean(
    bound_pattern::TypeBoundPattern,
    state::BinderState)
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

function lower_pattern_to_boolean(
    bound_pattern::AndBoundPattern,
    state::BinderState)
    :($(mapreduce(bp -> lower_pattern(bp, state),
            (a, b) -> :($a && $b),
            bound_pattern.subpatterns)))
end

function lower_pattern_to_boolean(
    bound_pattern::OrBoundPattern,
    state::BinderState)
    :($(mapreduce(bp -> lower_pattern(bp, state),
            (a, b) -> :($a || $b),
            bound_pattern.subpatterns)))
end

function fetch_pattern(x::Expr)
    Expr(:block, x, true)
end

function lower_pattern_to_boolean(
    bound_pattern::FetchFieldBoundPattern,
    state::BinderState)
    tempvar = state.assignments[bound_pattern]
    index = QuoteNode(bound_pattern.field_name)
    fetch_pattern(:($tempvar = getfield($(bound_pattern.input), $index)))
end

function lower_pattern_to_boolean(
    bound_pattern::FetchIndexBoundPattern,
    state::BinderState)
    tempvar = state.assignments[bound_pattern]
    i = bound_pattern.index
    if i < 0; i = :(length($(bound_pattern.input)) + $(i + 1)); end
    fetch_pattern(:($tempvar = getindex($(bound_pattern.input), $i)))
end

function lower_pattern_to_boolean(
    bound_pattern::FetchLengthBoundPattern,
    state::BinderState)
    tempvar = state.assignments[bound_pattern]
    fetch_pattern(:($tempvar = length($(bound_pattern.input))))
end

function lower_pattern_to_boolean(
    bound_pattern::RelationalBoundPattern,
    state::BinderState)
    @assert bound_pattern.relation == :>=
    :($(bound_pattern.relation)($(bound_pattern.input), $(esc(bound_pattern.value))))
end

function lower_pattern_to_boolean(
    bound_pattern::FetchRangeBoundPattern,
    state::BinderState)
    tempvar = state.assignments[bound_pattern]
    index = :($(bound_pattern.first_index):(length($(bound_pattern.input)) - $(bound_pattern.from_end)))
    fetch_pattern(:($(tempvar) = getindex($(bound_pattern.input), $(index))))
end

function lower_pattern_to_boolean(
    bound_pattern::FetchBindingBoundPattern,
    state::BinderState)
    tempvar = get_temp(bound_pattern.variable)
    fetch_pattern(:($(tempvar) = $(bound_pattern.input)))
end

function assignments(assigned::ImmutableDict{Symbol, Symbol})
    # produce a list of assignments to be splatted into the caller
    a = (:($(esc(patvar)) = $resultsym) for (patvar, resultsym) in assigned)
end

function lower_pattern_to_boolean(
    bound_pattern::WhereBoundPattern,
    state::BinderState)
    eval = esc(bound_pattern.source)
    block = Expr(:block, bound_pattern.location, eval)
    Expr(:let, Expr(:block, assignments(bound_pattern.assigned)...), block)
end

function lower_pattern(
    bound_pattern::BoundPattern,
    state::BinderState)
    Expr(:block, bound_pattern.location, lower_pattern_to_boolean(bound_pattern, state))
end
