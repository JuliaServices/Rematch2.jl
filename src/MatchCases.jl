struct MatchCaseResult
    location::LineNumberNode
    matched_expression::Any
    assigned::ImmutableDict{Symbol, Symbol}
    result_expression::Any
end

function handle_match_case(
    location::LineNumberNode,
    input_variable::Symbol,
    case,
    state::BinderState)
    assigned = ImmutableDict{Symbol, Symbol}()
    if @capture(case, pattern_ => result_)
        bound_pattern, assigned = bind_pattern!(
            location, pattern, input_variable, state, assigned)
        matched = lower_pattern_to_boolean(bound_pattern, state)
    else
        error("$(location.file):$(location.line): Unrecognized @match case syntax: `$case`.")
    end

    result0, assigned0 = subst_patvars(result, assigned)
    return MatchCaseResult(location, matched, assigned0, result0)
end

function handle_match_cases(location::LineNumberNode, mod::Module, value, match)
    if match isa Expr && match.head == :call && match.args[1] == :(=>)
        # previous version of @match supports `@match(expr, pattern => value)`
        match = Expr(:block, match)
    elseif !(match isa Expr) || match.head != :block
        error("$(location.file):$(location.line): Unrecognized @match block syntax: `$match`.")
    end

    input_variable = gensym("input_value")
    state = BinderState(mod, input_variable)
    cases = MatchCaseResult[]

    for case in match.args
        if case isa LineNumberNode
            location = case
        else
            push!(cases, handle_match_case(location, input_variable, case, state))
        end
    end

    tail = :($throw($MatchFailure($input_variable)))
    n = length(cases)
    for (i, case) in enumerate(reverse(cases))
        eval = Expr(:block, case.location, case.result_expression)
        tail = Expr(i == n ? :if : :elseif, case.matched_expression, eval, tail)
    end
    result = Expr(:block, state.assertions..., :($input_variable = $value), tail)
    # We use a `let` to mimic Rematch's closed scopes.
    result = Expr(:let, Expr(:block), result)
    esc(result)
end
