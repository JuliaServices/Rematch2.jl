#
# Bind a case, producing a case partial result.
#
function bind_case(
    case_number::Int,
    location::LineNumberNode,
    case,
    state::BinderState)::CasePartialResult
    if !(@capture(case, pattern_ => result_))
        error("$(location.file):$(location.line): Unrecognized @match2 case syntax: `$case`.")
    end

    bound_pattern, assigned = bind_pattern!(
        location, pattern, state.input_variable, state, ImmutableDict{Symbol, Symbol}())
    result0, assigned0 = subst_patvars(result, assigned)
    return CasePartialResult(case_number, location, pattern, bound_pattern, assigned0, result0)
end

#
# Build the state machine and return its entry point.
#
function build_state_machine_core(
    value,
    source_cases::Vector{Any},
    location::LineNumberNode,
    state::BinderState)::CodePoint
    cases = CasePartialResult[]
    for case in source_cases
        if case isa LineNumberNode
            location = case
        else
            bound_case::CasePartialResult = bind_case(length(cases) + 1, location, case, state)
            bound_case = simplify(bound_case, state)
            push!(cases, bound_case)
        end
    end

    # Track the set of reachable cases (by index)
    reachable = Set{Int}()

    # Make an entry point for the state machine
    entry = CodePoint(cases)

    # If the value had a type annotation, then we can use that to
    # narrow down the cases that we need to consider.
    if Base.isexpr(value, :(::), 2)
        type = value.args[2]
        bound_type = bind_type(location, type, state.input_variable, state)
        state.types[state.input_variable] = bound_type
        filter = BoundTypeTestPattern(location, type, state.input_variable, bound_type)
        entry = remove(filter, true, entry, state)
    else
        state.types[state.input_variable] = Any
    end

    # Build the state machine with the given entry point
    work_queue = Set{CodePoint}([entry])
    while !isempty(work_queue)
        code = pop!(work_queue)
        if code.action isa Nothing
            set_next!(code, state)
            @assert code.action !== nothing
            if code.action isa CasePartialResult
                push!(reachable, code.action.case_number)
            end
            next = code.next
            @assert next !== nothing
            succ = successors(code)
            for i in 1:length(succ)
                push!(work_queue, succ[i])
                # we will fall through to the true branch if possible and jump to
                # the false branch.  So we need a label for the false side.
                i != 1 && ensure_label!(succ[i], state)
            end
        else
            # We will need a label because it has
            # two predecessors in the generated code, and one of them will
            # have to jump to it rather than falling through
            next = code.next
            @assert next !== nothing
            ensure_label!(code, state)
        end
        @assert code.action !== nothing
    end

    # Warn if there were any unreachable cases
    for i in 1:length(cases)
        if !(i in reachable)
            case = cases[i]
            loc = case.location
            @warn("$(loc.file):$(loc.line): Case $(case.case_number): `$(case.pattern_source) =>` is not reachable.")
        end
    end

    entry
end

#
# Generate all of the code given the entry point
#
function generate_code(entry::CodePoint, value, location::LineNumberNode, state::BinderState)
    result_variable = gensym("match_result")
    result_label = gensym("completed")
    emit = Any[location, :($(state.input_variable) = $value)]
    togen = CodePoint[entry]
    generated = Set{CodePoint}()

    while !isempty(togen)
        pc = pop!(togen)
        pc in generated && continue
        push!(generated, pc)
        if pc.label isa Symbol
            push!(emit, :(@label $(pc.label)))
        end
        action = pc.action
        if action isa CasePartialResult
            # We've matched a pattern.
            push!(emit, action.location)
            push!(emit, :($result_variable = $(action.result_expression)))
            push!(emit, :(@goto $result_label))
        elseif action isa BoundFetchPattern
            push!(emit, action.location)
            push!(emit, code(action, state))
            (next::CodePoint,) = pc.next
            if next in generated
                @assert next.label isa Symbol
                push!(emit, :(@goto $(next.label)))
            else
                # we don't need a `goto` for the next state here:
                # it hasn't been generated yet, and we're pushing
                # it on the stack to be generated next.  We'll just
                # fall through to its code.
                push!(togen, next)
            end
        elseif action isa BoundTestPattern
            push!(emit, action.location)
            next_true, next_false = pc.next
            @assert next_false.label isa Symbol
            push!(emit, :($(code(action, state)) || @goto $(next_false.label)))
            push!(togen, next_false)
            if next_true in generated
                @assert next_true.label isa Symbol
                push!(emit, :(@goto $(next_true.label)))
            else
                # Push it on the stack of code to generate next, so
                # we can just fall into it instead of producing a `goto`.
                push!(togen, next_true)
            end
        elseif action isa Expr
            push!(emit, action)
        else
            error("this code point ($(typeof(action))) is believed unreachable")
        end
    end

    push!(emit, :(@label $result_label))
    push!(emit, result_variable)
    Expr(:block, state.assertions..., emit...)
end

#
# Build the whole state machine from the syntax for the value and body
#
function build_state_machine(location::LineNumberNode, mod::Module, value, body)
    if body isa Expr && body.head == :call && body.args[1] == :(=>)
        # previous version of @match supports `@match(expr, pattern => value)`
        body = Expr(:block, body)
    end
    if body isa Expr && body.head == :block
        source_cases = body.args
    elseif body isa Expr && body.head == :let && body.args[2] isa Expr && body.args[2].head == :block
        source_cases = body.args[2].args
    else
        error("$(location.file):$(location.line): Unrecognized @match2 block syntax: `$body`.")
    end

    state = BinderState(mod)
    entry = build_state_machine_core(value, source_cases, location, state)
    return (entry, state)
end

#
# Compute and record the next action for the given state.
#
function set_next!(code::CodePoint, state::BinderState)
    @assert code.action === nothing
    @assert code.next === nothing

    action::Union{CasePartialResult, BoundPattern, Expr} = next_action(code, state)
    next::Union{Tuple{}, Tuple{CodePoint}, Tuple{CodePoint, CodePoint}} =
        make_next(code, action, state)
    code.action = action
    code.next = next
    @assert !(code.next isa Nothing)
end


#
# Compute the next action for the given decision automatin state.  We take the
# simple approach of just doing the next thing on the list of the first pattern
# that might match (the left-to-right "heusristic").  We might use different
# heuristics to do better, but not likely by more than a few percent except
# in machine-generated code.
# See https://www.cs.tufts.edu/~nr/cs257/archive/norman-ramsey/match.pdf for details.
# See https://gist.github.com/gafter/145db4a2282296bdaa08e0a0dcce9217 for an example
# of machine-generated code that can cause an explosion of code size.
#
function next_action(
    code::CodePoint,
    state::BinderState)::Union{CasePartialResult, BoundPattern, Expr}
    if isempty(code.cases)
        # cases have been exhausted.  Return code to throw a match failure.
        return :($throw($MatchFailure($(state.input_variable))))
    end
    first_case = code.cases[1]
    if first_case.pattern isa BoundTruePattern
        # case has been satisfied.  Return it as our destination.
        return first_case
    end
    return next_action(first_case.pattern)
end
next_action(pattern::BoundPattern) = pattern
function next_action(pattern::Union{BoundFalsePattern, BoundTruePattern})
    error("unreachable - a $(typeof(pattern)) cannot be the next action")
end
function next_action(pattern::Union{BoundAndPattern, BoundOrPattern})
    return next_action(pattern.subpatterns[1])
end

#
# Given an action, make the "next" result, which is the action or successor
# state of the decision automaton.
#
function make_next(
    code::CodePoint,
    action::Union{CasePartialResult, Expr},
    state::BinderState)
    return ()
end
function make_next(
    code::CodePoint,
    action::BoundPattern,
    state::BinderState)
    error("pattern cannot be the next action: $(typeof(action))")
end
function intern(code::CodePoint, state::BinderState)
    @assert code.label === nothing
    newcode = get!(state.intern, code, code)
    if newcode !== code
        ensure_label!(newcode, state)
    end
    newcode
end
function make_next(
    code::CodePoint,
    action::BoundFetchPattern,
    state::BinderState)::Tuple{CodePoint}
    succ = remove(action, code, state)
    succ = intern(succ, state)
    return (succ,)
end

# When a test occurs, there are two subsequent states, depending on the outcome of the test.
function make_next(
    code::CodePoint,
    action::BoundTestPattern,
    state::BinderState)::Tuple{CodePoint, CodePoint}
    true_next = remove(action, true, code, state)
    false_next = remove(action, false, code, state)
    true_next = intern(true_next, state)
    false_next = intern(false_next, state)
    # we will fall through to the code for true if possible but jump to the code for false
    ensure_label!(false_next, state)
    return (true_next, false_next)
end

# The next code point is the same but without the action, since it has been done.
function remove(action::BoundFetchPattern, code::CodePoint, state::BinderState)::CodePoint
    cases = map(c -> remove(action, c, state), code.cases)
    succ = CodePoint(cases)

    # If we know the type of the fetched value, we can assert that in downstream code.
    bound_type = action.type
    if bound_type !== Any
        temp = get_temp(state, action)
        filter = BoundTypeTestPattern(action.location, action.source, temp, bound_type)
        succ = remove(filter, true, succ, state)
    end
    succ
end
function remove(action::BoundTestPattern, sense::Bool, code::CodePoint, state::BinderState)::CodePoint
    cases = map(c -> remove(action, sense, c, state), code.cases)
    return CodePoint(cases)
end

function remove(action::BoundTestPattern, sense::Bool, case::CasePartialResult, state::BinderState)::CasePartialResult
    with_pattern(case, remove(action, sense, case.pattern, state))
end
function remove(action::BoundFetchPattern, case::CasePartialResult, state::BinderState)::CasePartialResult
    with_pattern(case, remove(action, case.pattern, state))
end

#
# Remove the given action from a pattern.
#
remove(action::BoundFetchPattern, pattern::BoundPattern, state::BinderState)::BoundPattern = pattern
remove(action::BoundTestPattern, sense::Bool, pattern::BoundPattern, state::BinderState)::BoundPattern = pattern
function remove(action::BoundFetchPattern, pattern::BoundFetchPattern, state::BinderState)::BoundPattern
    return (action == pattern) ? BoundTruePattern(pattern.location, pattern.source) : pattern
end
function remove(action::BoundFetchPattern, pattern::Union{BoundAndPattern,BoundOrPattern}, state::BinderState)::BoundPattern
    subpatterns = collect(BoundPattern, map(p -> remove(action, p, state), pattern.subpatterns))
    return (typeof(pattern))(pattern.location, pattern.source, subpatterns)
end
function remove(action::BoundTestPattern, sense::Bool, pattern::BoundTestPattern, state::BinderState)::BoundPattern
    return (action == pattern) ? BoundBoolPattern(pattern.location, pattern.source, sense) : pattern
end
function remove(action::BoundEqualValueTestPattern, sense::Bool, pattern::BoundEqualValueTestPattern, state::BinderState)::BoundPattern
    if action.input != pattern.input
        return pattern
    end
    if isequal(action.value, pattern.value)
        return BoundBoolPattern(pattern.location, pattern.source, sense)
    end

    # As a special case, if the input variable is of type Bool, then we know that true and false
    # are the only values it can hold.
    type = state.types[action.input]
    if type == Bool && action.value isa Bool && pattern.value isa Bool
        @assert action.value != pattern.value # because we already checked for equality
        # If the one succeeded, then the other one fails
        return BoundBoolPattern(pattern.location, pattern.source, !sense)
    end

    return pattern
end
function remove(action::BoundTestPattern, sense::Bool, pattern::Union{BoundAndPattern,BoundOrPattern}, state::BinderState)::BoundPattern
    subpatterns = collect(BoundPattern, map(p -> remove(action, sense, p, state), pattern.subpatterns))
    return (typeof(pattern))(pattern.location, pattern.source, subpatterns)
end
function remove(action::BoundWhereTestPattern, sense::Bool, pattern::BoundWhereTestPattern, state::BinderState)::BoundPattern
    # Two where tests can be related by being the inverse of each other.
    action.input == pattern.input || return pattern
    replacement_value = (action.inverted == pattern.inverted) == sense
    return BoundBoolPattern(pattern.location, pattern.source, replacement_value)
end
function remove(action::BoundTypeTestPattern, sense::Bool, pattern::BoundTypeTestPattern, state::BinderState)::BoundPattern
    # Knowing the result of one type test can give information about another.  For
    # example, if you know `x` is a `String`, then you know that it isn't an `Int`.
    if (action == pattern)
        return BoundBoolPattern(pattern.location, pattern.source, sense)
    elseif action.input != pattern.input
        return pattern
    elseif sense
        # the type test succeeded.
        if action.type <: pattern.type
            return BoundTruePattern(pattern.location, pattern.source)
        elseif pattern.type <: action.type
            # we are asking about a narrower type - result unknown
            return pattern
        elseif typeintersect(pattern.type, action.type) == Base.Bottom
            # their intersection is empty, so it cannot be pattern.type
            return BoundFalsePattern(pattern.location, pattern.source)
        else
            return pattern
        end
    else
        # the type test failed.
        if action.type <: pattern.type
            # we are asking about a wider type - result unknown
            return pattern
        elseif pattern.type <: action.type
            # if it wasn't the wider type, then it won't be the narrower type
            return BoundFalsePattern(pattern.location, pattern.source)
        else
            return pattern
        end
    end
end

#
# Simplify a case by removing fetch operations whose results are not used.
#
function simplify(case::CasePartialResult, state::BinderState)::CasePartialResult
    required_temps = Set(values(case.assigned))
    simplified_pattern = simplify(case.pattern, required_temps, state)
    return with_pattern(case, simplified_pattern)
end

#
# Simplify a pattern by removing fetch operations whose results are not used.
#
function simplify(pattern::BoundPattern, required_temps::Set{Symbol}, state::BinderState)::BoundPattern
    push!(required_temps, pattern.input)
    pattern
end
function simplify(pattern::Union{BoundTruePattern, BoundFalsePattern}, required_temps::Set{Symbol}, state::BinderState)::BoundPattern
    pattern
end
function simplify(pattern::BoundFetchPattern, required_temps::Set{Symbol}, state::BinderState)::BoundPattern
    temp = get_temp(state, pattern)
    if temp in required_temps
        pop!(required_temps, temp)
        push!(required_temps, pattern.input)
        pattern
    else
        BoundTruePattern(pattern.location, pattern.source)
    end
end
function simplify(pattern::BoundFetchExpressionPattern, required_temps::Set{Symbol}, state::BinderState)::BoundPattern
    temp = get_temp(state, pattern)
    if temp in required_temps
        pop!(required_temps, temp)
        for (v, t) in pattern.assigned
            push!(required_temps, t)
        end
        pattern
    else
        BoundTruePattern(pattern.location, pattern.source)
    end
end
function simplify(pattern::BoundEqualValueTestPattern, required_temps::Set{Symbol}, state::BinderState)::BoundPattern
    push!(required_temps, pattern.input)
    for (v, t) in pattern.assigned
        push!(required_temps, t)
    end
    pattern
end
function simplify(pattern::BoundAndPattern, required_temps::Set{Symbol}, state::BinderState)::BoundPattern
    subpatterns = BoundPattern[]
    for p in reverse(pattern.subpatterns)
        push!(subpatterns, simplify(p, required_temps, state))
    end
    BoundAndPattern(pattern.location, pattern.source, BoundPattern[reverse(subpatterns)...])
end
function simplify(pattern::BoundOrPattern, required_temps::Set{Symbol}, state::BinderState)::BoundPattern
    subpatterns = BoundPattern[]
    new_required_temps = Set{Symbol}()
    for p in reverse(pattern.subpatterns)
        rt = copy(required_temps)
        push!(subpatterns, simplify(p, rt, state))
        union!(new_required_temps, rt)
    end
    empty!(required_temps)
    union!(required_temps, new_required_temps)
    BoundOrPattern(pattern.location, pattern.source, BoundPattern[reverse(subpatterns)...])
end

#
# Some useful macros for testing and diagnosing the state machine.
#

# Return the count of the number of states that would be generated by the match,
# but otherwise does not generate any code for the match.
macro match2_count_states(value, body)
    (entry, state) = build_state_machine(__source__, __module__, value, body)
    length(reachable_states(entry))
end

# Print the state maching (one line per state) to a given io channel
macro match2_dump(io, value, body)
    handle_match2_dump(__source__, __module__, io, value, body, false)
end
# Print the state maching (one line per state) to stdout
macro match2_dump(value, body)
    handle_match2_dump(__source__, __module__, stdout, value, body, false)
end
# Print the state maching (verbose) to a given io channel
macro match2_dumpall(io, value, body)
    handle_match2_dump(__source__, __module__, io, value, body, true)
end
# Print the state maching (verbose) to stdout
macro match2_dumpall(value, body)
    handle_match2_dump(__source__, __module__, stdout, value, body, true)
end

function handle_match2_dump(__source__, __module__, io, value, body, long)
    (entry, state) = build_state_machine(__source__, __module__, value, body)
    esc(:($dumpall($io, $entry, $state, $long)))
end

#
# Implementation of `@match2 value begin ... end`
#
function handle_match2_cases(location::LineNumberNode, mod::Module, value, body)
    (entry, state) = build_state_machine(location, mod, value, body)
    result = generate_code(entry, value, location, state)
    if body.head == :let
        result = Expr(:let, body.args[1], result)
    end
    esc(result)
end
