struct MatchCaseResult
    location::LineNumberNode
    assigned::ImmutableDict{Symbol, Symbol}
    matched_expression::Any
    result_expression::Any
end

function handle_match_case(
    location::LineNumberNode,
    input_variable::Symbol,
    case,
    state::BinderState)
    assigned = ImmutableDict{Symbol,Symbol}()
    if @capture(case, pattern_ => result_)
        (bound_pattern, assigned) = bind_pattern!(
            location, pattern, input_variable, state, assigned)
        matched = lower_pattern_to_boolean(bound_pattern, state)
    else
        error("$(location.file):$(location.line): Unrecognized @match case syntax: `$case`.")
    end

    return MatchCaseResult(location, assigned, matched, result)
end

function handle_match_cases(location::LineNumberNode, mod::Module, value, match)
    if (match isa Expr && match.head == :call && match.args[1] == :(=>))
        # previous version of @match supports `@match(expr, pattern => value)`
        match = Expr(:block, match)
    elseif !(match isa Expr) || match.head != :block
        error("$(location.file):$(location.line): Unrecognized @match block syntax: `$match`.")
    end
    state = BinderState(mod)
    input_variable::Symbol = gensym("input_value")
    cases = MatchCaseResult[]

    for case in match.args
        if case isa LineNumberNode
            location = case
        else
            push!(cases, handle_match_case(location, input_variable, case, state))
        end
    end

    tail = :(throw(MatchFailure($input_variable)))
    n = length(cases)
    for (i, case) in enumerate(reverse(cases))
        eval = Expr(:block, case.location, esc(case.result_expression))
        result = Expr(:let, Expr(:block, assignments(case.assigned)...), eval)
        tail = Expr(i == n ? :if : :elseif, case.matched_expression, result, tail)
    end
    Expr(:block, state.assertions..., :($input_variable = $(esc(value))), tail)
end
