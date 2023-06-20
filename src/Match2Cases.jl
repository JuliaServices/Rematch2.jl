#
# A data structure representing information about a match case in one
# state of the decision automaton for a @match block statement.
# Given a statement
#
# @match expression begin
#     pattern => result
#     ....
# end
#
# The bound pattern represents the remaining operations needed to
# decide if the pattern matches.
#
struct CasePartialResult
    # The index of the case, starting with 1 for the first => in the @match
    case_number::Int

    # Its location for error reporting purposes
    location::LineNumberNode

    # Its source for error reporting
    pattern_source

    # The set of remaining operations required to perform the match.
    # In this state, some operations may have already been done, and they
    # are removed from the bound pattern.  When the bound pattern is simply
    # `true`, it has matched.
    pattern::BoundPattern

    # The set of user variables to assign when the match succeeds;
    # they might be used in the result expression.
    assigned::ImmutableDict{Symbol, Symbol}

    # The user's result expression for this case.
    result_expression::Any

    _cached_hash::UInt64
    function CasePartialResult(
        case_number::Int,
        location::LineNumberNode,
        pattern_source,
        pattern::BoundPattern,
        assigned::ImmutableDict{Symbol, Symbol},
        result_expression::Any)
        _hash = hash((case_number, pattern, assigned), 0x1cdd9657bfb1e645)
        new(case_number, location, pattern_source, pattern, assigned, result_expression, _hash)
    end
end
function with_pattern(
    case::CasePartialResult,
    new_pattern::BoundPattern)
    CasePartialResult(
        case.case_number,
        case.location,
        case.pattern_source,
        new_pattern,
        case.assigned,
        case.result_expression)
end
function Base.hash(case::CasePartialResult, h::UInt64)
    hash(case._cached_hash, h)
end
Base.hash(case::CasePartialResult) = case._cached_hash
function Base.:(==)(a::CasePartialResult, b::CasePartialResult)
    a._cached_hash == b._cached_hash &&
    isequal(a.case_number, b.case_number) &&
        isequal(a.pattern, b.pattern) &&
        isequal(a.assigned, b.assigned)
end
pretty(x) = sprint(pretty, x)
function pretty(io::IO, case::CasePartialResult, state::BinderState)
    print(io, case.case_number, ": ")
    pretty(io, case.pattern, state)
    print(io, " => ")
    print(io, case.result_expression)
end
function pretty(io::IO, d::AbstractDict{K, V}) where { K, V }
    print(io, "[")
    join(io, d, ", ")
    print(io, "]")
end
pretty(io::IO, p::BoundPattern, state::BinderState) = pretty(io, p)
function pretty(io::IO, p::BoundFetchPattern, state::BinderState)
    temp = get_temp(state, p)
    print(io, temp, "=")
    pretty(io, p)
end
pretty(io::IO, ::BoundTruePattern) = print(io, "true")
pretty(io::IO, ::BoundFalsePattern) = print(io, "false")
pretty(io::IO, p::BoundEqualValueTestPattern) = print(io, p.input, " == ", p.value)
pretty(io::IO, p::BoundRelationalTestPattern) = print(io, p.input, " ", p.relation, " ", p.value)
function pretty(io::IO, p::BoundWhereTestPattern)
    print(io, "where ")
    isempty(p.assigned) || pretty(io, p.assigned)
    print(io, p.source)
end
pretty(io::IO, p::BoundTypeTestPattern) = print(io, p.input, " isa ", p.type)
function pretty(io::IO, p::Union{BoundOrPattern, BoundAndPattern}, state::BinderState)
    op = (p isa BoundOrPattern) ? "||" : "&&"
    print(io, "(")
    first = true
    for sp in p.subpatterns
        first || print(io, " ", op, " ")
        first = false
        pretty(io, sp, state)
    end
    print(io, ")")
end
pretty(io::IO, p::BoundFetchFieldPattern) = print(io, p.input, ".", p.field_name)
pretty(io::IO, p::BoundFetchIndexPattern) = print(io, p.input, "[", p.index, "]")
pretty(io::IO, p::BoundFetchRangePattern) = print(io, p.input, "[", p.first_index, ":(length(", p.input, ")-", p.from_end, ")]")
pretty(io::IO, p::BoundFetchLengthPattern) = print(io, "length(", p.input, ")")
pretty(io::IO, p::BoundFetchBindingPattern) = print(io, p.input)

#
# Simplify a pattern by removing fetch operations that are not used.
#
function simplify(pattern::BoundPattern, required_temps::Set{Symbol}, state::BinderState)
    push!(required_temps, pattern.input)
    pattern
end
function simplify(pattern::Union{BoundTruePattern, BoundFalsePattern}, required_temps::Set{Symbol}, state::BinderState)
    pattern
end
function simplify(pattern::BoundFetchPattern, required_temps::Set{Symbol}, state::BinderState)
    temp = get_temp(state, pattern)
    if temp in required_temps
        pop!(required_temps, temp)
        push!(required_temps, pattern.input)
        pattern
    else
        BoundTruePattern(pattern.location, pattern.source)
    end
end
function simplify(pattern::BoundWhereTestPattern, required_temps::Set{Symbol}, state::BinderState)
    for (_, t) in pattern.assigned
        push!(required_temps, t)
    end
    pattern
end
function simplify(pattern::BoundEqualValueTestPattern, required_temps::Set{Symbol}, state::BinderState)
    push!(required_temps, pattern.input)
    for (_, t) in pattern.assigned
        push!(required_temps, t)
    end
    pattern
end
function simplify(pattern::BoundAndPattern, required_temps::Set{Symbol}, state::BinderState)
    subpatterns = BoundPattern[]
    for p in reverse(pattern.subpatterns)
        push!(subpatterns, simplify(p, required_temps, state))
    end
    BoundAndPattern(pattern.location, pattern.source, BoundPattern[reverse(subpatterns)...])
end
function simplify(pattern::BoundOrPattern, required_temps::Set{Symbol}, state::BinderState)
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
# Simplify a case by removing fetch operations that are not used.
#
function simplify(case::CasePartialResult, state::BinderState)
    required_temps = Set(values(case.assigned))
    simplified_pattern = simplify(case.pattern, required_temps, state)
    return with_pattern(case, simplified_pattern)
end

macro _const(x)
    if VERSION >= v"1.8"
        Expr(:const, esc(x))
    else
        esc(x)
    end
end

#
# A state of the decision automaton (i.e. a point in the generated code),
# which is a set of partially matched cases.
#
mutable struct CodePoint
    # The state of the cases.  Impossible cases, which are designated by a
    # `false` `bound_pattern`, are removed from this array.  Cases are always
    # ordered by `case_number`.
    @_const cases::ImmutableVector{CasePartialResult}

    # A label to produce in the code at entry to the code where
    # this state is implemented, if one is needed.
    label::Union{Nothing, Symbol}

    # The selected action to take from this state: either
    # - Nothing, before it has been computed, or
    # - Case whose tests have all passed, or
    # - A bound pattern to perform and then move on to the next state, or
    # - An Expr to insert into the code when all else is exhausted
    #   (which throws MatchFailure)
    action::Union{Nothing, CasePartialResult, BoundPattern, Expr}

    # The next code point(s):
    # - Nothing before being computed
    # - Tuple{} if the action is a case which was matched or a MatchFailure
    # - Tuple{CodePoint} if the action was a fetch pattern. It designates
    #   the code to perform after the fetch.
    # - Tuple{CodePoint, CodePoint} if the action is a test.  These are the states
    #   to go to if the result of the test is true ([1]) or false ([2]).
    next::Union{Nothing, Tuple{}, Tuple{CodePoint}, Tuple{CodePoint, CodePoint}}

    _cached_hash::UInt64
end
function CodePoint(cases::Vector{CasePartialResult})
    CodePoint(ImmutableVector(cases), nothing, nothing, nothing,
        hash(cases, 0xc98a9a23c2d4d915))
end
Base.hash(case::CodePoint, h::UInt64) = hash(case._cached_hash, h)
Base.hash(case::CodePoint) = case._cached_hash
function Base.:(==)(a::CodePoint, b::CodePoint)
    a === b ||
        a._cached_hash == b._cached_hash &&
        isequal(a.cases, b.cases)
end
function with_cases(code::CodePoint, cases::Vector{CasePartialResult})
    cases = filter(case -> !(case.pattern isa BoundFalsePattern), cases)
    for i in eachindex(cases)
        if is_irrefutable(cases[i].pattern)
            cases = cases[1:i]
            break
        end
    end
    CodePoint(cases)
end
function ensure_label!(code::CodePoint)
    if code.label isa Nothing
        code.label = gensym("label")
    end
end
function simple_name(code::CodePoint)
    h::UInt = hash(code)
    io = IOBuffer()
    for i in 1:5
        x = rem(h, 27)
        if x != 0; print(io, ('a':'z')[x]); end
        h = div(h, 27)
    end
    String(take!(io))
end
function name(code::CodePoint)
    name = simple_name(code)
    if code.label isa Nothing
        "CodePoint $name"
    else
        "CodePoint $name ($(code.label))"
    end
end
function pretty(io::IO, code::CodePoint, state::BinderState)
    println(io, name(code))
    for case in code.cases
        print(io, "  ")
        pretty(io, case, state)
        println(io)
    end
    print(io, "    action: ")
    action = code.action
    if action isa Nothing
        println(io, "not computed")
    elseif action isa CasePartialResult
        print(io, "completed case ", action.case_number, " with value ")
        isempty(action.assigned) || pretty(io, action.assigned)
        println(io, " ", action.result_expression)
    elseif action isa BoundPattern
        print(io, "execute pattern ")
        pretty(io, action, state)
        println(io)
    elseif action isa Expr
        println(io, "execute code ", action)
    else
        error("unknown action")
    end
    next = code.next
    if next isa Tuple{CodePoint}
        print(io, "    next: $(name(next[1]))")
        println(io)
    elseif next isa Tuple{CodePoint, CodePoint}
        print(io, "    when true: $(name(next[1]))")
        println(io)
        print(io, "    when false: $(name(next[2]))")
        println(io)
    end
end
function successors(c::CodePoint)::Vector{CodePoint}
    @assert !(c.next isa Nothing)
    collect(c.next)
end
function reachable_states(root::CodePoint)::Vector{CodePoint}
    topological_sort(successors, [root])
end

function dumpall(io::IO, root::CodePoint, state::BinderState)
    all::Vector{CodePoint} = reachable_states(root)
    println(io)
    println(io, "State Machine:")
    for code in all
        pretty(io, code, state)
    end
    println(io, "end # of state machine")
end
#
# Print the entire state machine to `stdout`.  Useful for debugging the state
# machine and looking for optimization opportunities.
#
dumpall(root::CodePoint, state::BinderState) = dumpall(stdout, root, state)

#
# Bind a case, producing a case partial result.
#
function bind_case(
    case_number::Int,
    location::LineNumberNode,
    case,
    state::BinderState)::CasePartialResult
    while case isa Expr && case.head == :macrocall
        # expand top-level macros only
        case = macroexpand(state.mod, case, recursive=false)
    end

    is_expr(case, :call, 3) && case.args[1] == :(=>) ||
        error("$(location.file):$(location.line): Unrecognized @match2 case syntax: `$case`.")
    pattern = case.args[2]
    result = case.args[3]
    (pattern, result) = adjust_case_for_return_macro(state.mod, pattern, result)
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
                i != 1 && ensure_label!(succ[i])
            end
        else
            # We will need a label because it has
            # two predecessors in the generated code, and one of them will
            # have to jump to it rather than falling through
            next = code.next
            @assert next !== nothing
            ensure_label!(code)
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

# Generate all of the code given the entry point
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

    input_variable = gensym("input_value")
    state = BinderState(mod, input_variable)
    entry = build_state_machine_core(value, source_cases, location, state)

    return (entry, state)
end

# For testing purposes, this macro permits the caller to determine the number of states
macro match2_count_states(value, body)
    entry, _ = build_state_machine(__source__, __module__, value, body)
    # We return a number into the caller's code.
    count_states(entry)
end
function count_states(entry::CodePoint)
    tocount = CodePoint[entry]
    counted = Set{CodePoint}()
    total::Int = 0
    while !isempty(tocount)
        pc = pop!(tocount)
        pc in counted && continue
        push!(counted, pc)
        total += 1
        if pc.next isa Tuple{CodePoint}
            push!(tocount, pc.next[1])
        elseif pc.next isa Tuple{CodePoint, CodePoint}
            push!(tocount, pc.next[1])
            push!(tocount, pc.next[2])
        end
    end
    total
end

# For debugging purposes, these macros print the state machine.
macro match2_dump_states(io, value, body)
    entry, state = build_state_machine(__source__, __module__, value, body)
    esc(quote
        $dumpall($io, $entry, $state)
        $(count_states(entry))
    end)
end
macro match2_dump_states(value, body)
    entry, state = build_state_machine(__source__, __module__, value, body)
    esc(quote
        $dumpall($stdout, $entry, $state)
        $(count_states(entry))
    end)
end

function handle_match2_cases(location::LineNumberNode, mod::Module, value, body)
    entry, state = build_state_machine(location, mod, value, body)
    result = generate_code(entry, value, location, state)
    if body.head == :let
        result = Expr(:let, body.args[1], result)
    end
    esc(result)
end

next_action(pattern::BoundPattern) = pattern
function next_action(pattern::Union{BoundFalsePattern, BoundTruePattern})
    error("unreachable - a $(typeof(pattern)) cannot be the next action")
end
function next_action(pattern::Union{BoundAndPattern, BoundOrPattern})
    return next_action(pattern.subpatterns[1])
end
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
function set_next!(code::CodePoint, state::BinderState)
    @assert code.action === nothing
    @assert code.next === nothing

    action::Union{CasePartialResult, BoundPattern, Expr} = next_action(code, state)
    next::Union{Tuple{}, Tuple{CodePoint}, Tuple{CodePoint, CodePoint}} =
        make_next(code, action, state)
    code.action = action
    code.next = next
    @assert code.next !== nothing
end

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
        ensure_label!(newcode)
    end
    newcode
end
function make_next(
    code::CodePoint,
    action::BoundFetchPattern,
    state::BinderState)::Tuple{CodePoint}
    cases = map(case -> remove(action, case), code.cases)
    succ = with_cases(code, cases)
    succ = intern(succ, state)
    return (succ,)
end
function remove(action::BoundFetchPattern, pattern::BoundPattern)
    pattern
end
function remove(action::BoundFetchPattern, pattern::BoundFetchPattern)
    return (action == pattern) ? BoundTruePattern(pattern.location, pattern.source) : pattern
end
function remove(action::BoundFetchPattern, pattern::BoundAndPattern)
    subpatterns = collect(BoundPattern, map(p -> remove(action, p), pattern.subpatterns))
    return BoundAndPattern(pattern.location, pattern.source, subpatterns)
end
function remove(action::BoundFetchPattern, pattern::BoundOrPattern)
    subpatterns = collect(BoundPattern, map(p -> remove(action, p), pattern.subpatterns))
    return BoundOrPattern(pattern.location, pattern.source, subpatterns)
end
function remove(action::BoundFetchPattern, case::CasePartialResult)
    return with_pattern(case, remove(action, case.pattern))
end

# When a test occurs, there are two subsequent states, depending on the outcome of the test.
function make_next(
    code::CodePoint,
    action::BoundTestPattern,
    state::BinderState)::Tuple{CodePoint, CodePoint}
    true_next = remove(action, true, code)
    false_next = remove(action, false, code)
    # simplify each state to remove unnecessary cases
    true_next = intern(true_next, state)
    false_next = intern(false_next, state)
    # we will fall through to the code for true but jump to the code for false
    ensure_label!(false_next)
    return (true_next, false_next)
end
function remove(action::BoundTestPattern, sense::Bool, code::CodePoint)::CodePoint
    cases = map(c -> remove(action, sense, c), code.cases)
    return with_cases(code, cases)
end
function remove(action::BoundTestPattern, sense::Bool, case::CasePartialResult)
    return with_pattern(case, remove(action, sense, case.pattern))
end
# Make a copy of `pattern` with the removal of any parts that are redundant given
# that `action` is known to have a result `sense`.
function remove(action::BoundTestPattern, sense::Bool, pattern::BoundPattern)::BoundPattern
    error("not implemented: remove(::$(typeof(action)), ::$(typeof(sense)), ::$(typeof(pattern)))")
end
function remove(action::BoundTestPattern, sense::Bool, pattern::Union{BoundTruePattern, BoundFalsePattern, BoundFetchPattern})::BoundPattern
    pattern
end
function remove(action::BoundTestPattern, sense::Bool, pattern::BoundAndPattern)::BoundPattern
    BoundAndPattern(pattern.location, pattern.source,
        collect(BoundPattern, map(p -> remove(action, sense, p)::BoundPattern, pattern.subpatterns)))
end
function remove(action::BoundTestPattern, sense::Bool, pattern::BoundOrPattern)::BoundPattern
    BoundOrPattern(pattern.location, pattern.source,
        collect(BoundPattern, map(p -> remove(action, sense, p)::BoundPattern, pattern.subpatterns)))
end
function remove(action::BoundTestPattern, sense::Bool, pattern::BoundTestPattern)::BoundPattern
    if action == pattern
        BoundBoolPattern(pattern.location, pattern.source, sense)
    else
        pattern
    end
end
function remove(action::BoundTypeTestPattern, sense::Bool, pattern::BoundTypeTestPattern)::BoundPattern
    if action == pattern
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
