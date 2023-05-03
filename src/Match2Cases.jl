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
        pattern::BoundPattern,
        assigned::ImmutableDict{Symbol, Symbol},
        result_expression::Any)
        _hash = hash((case_number, pattern, assigned), 0x1cdd9657bfb1e645)
        new(case_number, location, pattern, assigned, result_expression, _hash)
    end
end
function with_pattern(
    case::CasePartialResult,
    new_pattern::BoundPattern)
    CasePartialResult(
        case.case_number,
        case.location,
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
function pretty(x)
    io = IOBuffer()
    pretty(io, x)
    String(take!(io))
end
function pretty(io::IO, case::CasePartialResult, state::BinderState)
    print(io, case.case_number, ": ")
    pretty(io, case.pattern, state)
    print(io, " => ")
    print(io, case.result_expression)
end
function pretty(io::IO, d::AbstractDict{K, V}) where { K, V }
    print(io, "[")
    first = true
    for pair in d
        first || print(io, ", ")
        first = false
        show(io, pair)
    end
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
pretty(io::IO, p::BoundRelationalTestPattern) = print(io, p.input, " ", p.relation, ", ", p.value)
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
    for (v, t) in pattern.assigned
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

    # When the cases have been exhausted in the state machine, the cases in the tail
    # are then handled.  This offers a simple way to build the state machine as a
    # directed acyclic graph, for example to handle "or" patterns.
    @_const tail::Union{Nothing, CodePoint}

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
function CodePoint(cases::Vector{CasePartialResult}, tail::Union{Nothing, CodePoint} = nothing)
    CodePoint(ImmutableVector(cases), tail, nothing, nothing, nothing,
        hash((cases, tail), 0xc98a9a23c2d4d915))
end
Base.hash(case::CodePoint, h::UInt64) = hash(case._cached_hash, h)
Base.hash(case::CodePoint) = case._cached_hash
function Base.:(==)(a::CodePoint, b::CodePoint)
    a === b ||
        a._cached_hash == b._cached_hash &&
        isequal(a.cases, b.cases) &&
        isequal(a.tail, b.tail)
end
function with_cases(code::CodePoint, cases::Vector{CasePartialResult})
    (isempty(cases) && code.tail isa CodePoint) ? code.tail : CodePoint(cases, code.tail)
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
    if code.tail isa CodePoint
        println(io, "  ... for more cases see $(name(code.tail))")
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
    @assert c.tail isa Nothing
    collect(c.next)
end
function reachable_states(root::CodePoint)::Vector{CodePoint}
    topological_sort([root], successors)
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
    state::BinderState)
    if !(@capture(case, pattern_ => result_))
        error("$(location.file):$(location.line): Unrecognized @match2 case syntax: `$case`.")
    end

    (bound_pattern, assigned) = bind_pattern!(
        location, pattern, state.input_variable, state, ImmutableDict{Symbol,Symbol}())
    return CasePartialResult(case_number, location, bound_pattern, assigned, result)
end

#
# Build the state machine and return its entry point.
#
function build_state_machine(value, match, location::LineNumberNode, state::BinderState)::CodePoint
    cases = CasePartialResult[]
    for case in match.args
        if case isa LineNumberNode
            location = case
        else
            case = bind_case(length(cases) + 1, location, case, state)
            case = simplify(case, state)
            push!(cases, case)
        end
    end
    entry = CodePoint(cases)

    work_queue = Set{CodePoint}([entry])
    while !isempty(work_queue)
        code = pop!(work_queue)
        if code.action isa Nothing
            set_next!(code, state)
            @assert !(code.action isa Nothing)
            next = code.next
            @assert !(next isa Nothing)
            succ = successors(code)
            for i in 1:length(succ)
                push!(work_queue, succ[i])
                # we will fall through to the true branch if possible and jump to
                # the false branch.  So we need a label for the false side.
                if i != 1; ensure_label!(succ[i]); end
            end
        else
            # code may have been processed already, for example
            # when handling a common tail between two states, which happens
            # for an "or" pattern.  We will need a label because it has
            # two predecessors in the generated code, and one of them will
            # have to jump to it rather than falling through
            next = code.next
            @assert !(next isa Nothing)
            ensure_label!(code)
        end
        @assert !(code.action isa Nothing)
    end

    entry
end

function generate_code(entry::CodePoint, value, state::BinderState)
    result_variable = gensym("match_result")
    result_label = gensym("completed")
    emit = Any[:($(state.input_variable) = $(esc(value)))]
    togen = CodePoint[entry]
    generated = Set{CodePoint}()

    while !isempty(togen)
        pc = pop!(togen)
        if pc in generated; continue; end
        push!(generated, pc)
        if pc.label isa Symbol
            push!(emit, :(@label $(pc.label)))
        end
        action = pc.action
        if action isa CasePartialResult
            # We've matched a pattern.
            result = :($result_variable = $(esc(action.result_expression)))
            if !isempty(action.assigned)
                result = Expr(:let, Expr(:block, assignments(action.assigned)...), result)
            end
            push!(emit, action.location)
            push!(emit, result)
            push!(emit, :(@goto $result_label))
        elseif action isa BoundFetchPattern
            push!(emit, code(action, state))
            (next::CodePoint,) = pc.next
            if next in generated
                @assert next.label isa Symbol
                push!(emit, :(@goto $(next.label)))
            else
                push!(togen, next)
            end
        elseif action isa BoundTestPattern
            (next_true, next_false) = pc.next
            @assert next_false.label isa Symbol
            push!(emit, :($(code(action, state)) || @goto $(next_false.label)))
            push!(togen, next_false)
            if next_true in generated
                @assert next_true.label isa Symbol
                push!(emit, :(@goto $(next_true.label)))
            else
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
    Expr(:let, Expr(:block, :($result_variable = nothing)), Expr(:block, state.assertions..., emit...))
end

function handle_match2_cases(location::LineNumberNode, mod::Module, value, match)
    if (match isa Expr && match.head == :call && match.args[1] == :(=>))
        # previous version of @match supports `@match(expr, pattern => value)`
        match = Expr(:block, match)
    elseif !(match isa Expr) || match.head != :block
        error("$(location.file):$(location.line): Unrecognized @match2 block syntax: `$match`.")
    end

    input_variable::Symbol = gensym("input_value")
    state = BinderState(mod, input_variable)
    entry = build_state_machine(value, match, location, state)

    #
    # When diagnosing or optimizing the pattern-matching machinery, comment out the
    # following `dumpall(entry)` statement to see what the state machine looks like
    # and how it is computed.  In this form it is easy to find optimization
    # opportunities.
    #
    # dumpall(entry, state)

    generate_code(entry, value, state)
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
        @assert code.tail isa Nothing
        # cases have been exhausted.  Return code to throw a match failure.
        return :(throw(MatchFailure($(state.input_variable))))
    end
    first_case = code.cases[1]
    if first_case.pattern isa BoundTruePattern
        # case has been satisfied.  Return it as our destination.
        return first_case
    end
    return next_action(first_case.pattern)
end
function set_next!(code::CodePoint, state::BinderState)
    @assert code.action isa Nothing
    @assert code.next isa Nothing

    action::Union{CasePartialResult, BoundPattern, Expr} = next_action(code, state)
    next::Union{Tuple{}, Tuple{CodePoint}, Tuple{CodePoint, CodePoint}} =
        make_next(code, action, state)
    code.action = action
    code.next = next
    @assert !(code.next isa Nothing)
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
    cases = filter(case -> !(case.pattern isa BoundFalsePattern), cases)
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

const TrueSense = Val(true)
const FalseSense = Val(false)
const Sense = Union{Val{true}, Val{false}}
# When a test occurs, there are two subsequent states, depending on the outcome of the test.
function make_next(
    code::CodePoint,
    action::BoundTestPattern,
    state::BinderState)::Tuple{CodePoint, CodePoint}
    true_next = remove(action, TrueSense, code)
    false_next = remove(action, FalseSense, code)
    # simplify each state to remove unnecessary cases
    true_next = simplify(true_next)
    false_next = simplify(false_next)
    # TODO: Remove a common tail between the two successor states
    true_next = intern(true_next, state)
    false_next = intern(false_next, state)
    ensure_label!(false_next)
    return (true_next, false_next)
end
any_tests(pattern::Union{BoundFetchPattern, BoundTruePattern}) = false
any_tests(pattern::BoundTestPattern) = true
any_tests(pattern::Union{BoundAndPattern, BoundOrPattern}) = any(any_tests, pattern.subpatterns)
function simplify(code::CodePoint)
    if length(code.cases) > 1 && !any_tests(code.cases[1].pattern)
        with_cases(code, [code.cases[1]])
    else
        code
    end
end
function remove(action::BoundTestPattern, sense::Sense, code::CodePoint)::CodePoint
    cases = map(c -> remove(action, sense, c), code.cases)
    cases = filter(case -> !(case.pattern isa BoundFalsePattern), cases)
    return with_cases(code, cases)
end
function remove(action::BoundTestPattern, sense::Sense, case::CasePartialResult)
    return with_pattern(case, remove(action, sense, case.pattern))
end
# Make a copy of `pattern` with the removal of any parts that are redundant given
# that `action` is known to have a result `sense`.
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundPattern)::BoundPattern
    error("not implemented: remove(::$(typeof(action)), ::$(typeof(sense)), ::$(typeof(pattern)))")
end
function remove(action::BoundTestPattern, sense::Sense, pattern::Union{BoundTruePattern, BoundFalsePattern, BoundFetchPattern})::BoundPattern
    pattern
end
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundAndPattern)::BoundPattern
    BoundAndPattern(pattern.location, pattern.source,
        collect(BoundPattern, map(p -> remove(action, sense, p)::BoundPattern, pattern.subpatterns)))
end
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundOrPattern)::BoundPattern
    BoundOrPattern(pattern.location, pattern.source,
        collect(BoundPattern, map(p -> remove(action, sense, p)::BoundPattern, pattern.subpatterns)))
end
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundTestPattern)::BoundPattern
    if (action == pattern)
        BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
    else
        pattern
    end
end
function remove(action::BoundTypeTestPattern, sense::Sense, pattern::BoundTypeTestPattern)::BoundPattern
    succeeded = typeof(sense).parameters[1]
    if (action == pattern)
        return BoundBoolPattern(pattern.location, pattern.source, succeeded)
    elseif action.input != pattern.input
        return pattern
    elseif succeeded
        # the type test succeeded.
        if action.type <: pattern.type
            return BoundBoolPattern(pattern.location, pattern.source, true)
        elseif pattern.type <: action.type
            # we are asking about a narrower type - result unknown
            return pattern
        elseif typeintersect(pattern.type, action.type) == Base.Bottom
            # their intersection is empty, so it cannot be pattern.type
            return BoundBoolPattern(pattern.location, pattern.source, false)
        end
    else
        # the type test failed.
        if action.type <: pattern.type
            # we are asking about a wider type - result unknown
            return pattern
        elseif pattern.type <: action.type
            # if it wasn't the wider type, then it won't be the narrower type
            return BoundBoolPattern(pattern.location, pattern.source, false)
        else
            return pattern
        end
    end
end
# function remove(action::BoundRelationalTestPattern, sense::Sense, pattern::BoundRelationalTestPattern)::BoundPattern
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         @assert action.relation == :>=
#         @assert pattern.relation == :>=
#         error("not implemented")
#     else
#         pattern
#     end
# end
# function remove(action::BoundEqualValueTestPattern, sense::Sense, pattern::BoundEqualValueTestPattern)::BoundPattern
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         error("not implemented")
#     else
#         pattern
#     end
# end
# function remove(action::BoundEqualValueTestPattern, sense::Sense, pattern::BoundRelationalTestPattern)::BoundPattern
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         error("not implemented")
#     else
#         pattern
#     end
# end
# function remove(action::BoundRelationalTestPattern, sense::Sense, pattern::BoundEqualValueTestPattern)::BoundPattern
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         error("not implemented")
#     else
#         pattern
#     end
# end
