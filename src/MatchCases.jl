struct MatchCaseResult
    location::LineNumberNode
    matched_expression::Any
    result_expression::BoundExpression
end

function handle_match_case(
    location::LineNumberNode,
    input_variable::Symbol,
    case,
    binder::BinderContext)
    assigned = ImmutableDict{Symbol, Symbol}()

    is_expr(case, :call, 3) && case.args[1] == :(=>) ||
        error("$(location.file):$(location.line): Unrecognized @match case syntax: `$case`.")
    pattern = case.args[2]
    result = case.args[3]
    (bound_pattern, assigned) = bind_pattern!(
        location, pattern, input_variable, binder, assigned)
    matched = lower_pattern_to_boolean(bound_pattern, binder)

    result_expression = bind_expression(location, result, assigned)
    return MatchCaseResult(location, matched, result_expression)
end

function handle_match_cases(location::LineNumberNode, mod::Module, value, match)
    if match isa Expr && match.head == :call && match.args[1] == :(=>)
        # previous version of @match supports `@match(expr, pattern => value)`
        match = Expr(:block, match)
    elseif !(match isa Expr) || match.head != :block
        error("$(location.file):$(location.line): Unrecognized @match block syntax: `$match`.")
    end

    binder = BinderContext(mod)
    input_variable::Symbol = binder.input_variable
    cases = MatchCaseResult[]

    for case in match.args
        if case isa LineNumberNode
            location = case
        else
            push!(cases, handle_match_case(location, input_variable, case, binder))
        end
    end

    tail = :($throw($MatchFailure($input_variable)))
    n = length(cases)
    for (i, case) in enumerate(reverse(cases))
        eval = Expr(:block, case.location, code(case.result_expression))
        tail = Expr(i == n ? :if : :elseif, case.matched_expression, eval, tail)
    end
    result = Expr(:block, binder.assertions..., :($input_variable = $value), tail)
    # We use a `let` to mimic Rematch's closed scopes.
    result = Expr(:let, Expr(:block), result)
    esc(result)
end
