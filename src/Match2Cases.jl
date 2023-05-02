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
function pretty(io::IO, case::CasePartialResult)
    print(io, case.case_number, ": ")
    pretty(io, case.pattern)
    if !isempty(case.assigned)
        print(io, " ")
        pretty(io, case.assigned)
    end
    print(io, " (=> ")
    print(io, case.result_expression)
    print(io, ")")
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
pretty(io::IO, ::BoundTruePattern) = print(io, "true")
pretty(io::IO, ::BoundFalsePattern) = print(io, "false")
pretty(io::IO, p::BoundEqualValueTestPattern) = print(io, p.input, "==", p.value)
pretty(io::IO, p::BoundRelationalTestPattern) = print(io, p.input, p.relation, p.value)
pretty(io::IO, p::BoundWhereTestPattern) = print(io, p.source)
pretty(io::IO, p::BoundTypeTestPattern) = print(io, p.input, " isa ", p.type)
op(p::BoundOrPattern) = "OR"
op(p::BoundAndPattern) = "AND"
function pretty(io::IO, p::Union{BoundOrPattern, BoundAndPattern})
    print(io, op(p))
    print(io, "[")
    first = true
    for sp in p.subpatterns
        if sp isa BoundFetchBindingPattern; continue; end
        first || print(io, ", ")
        first = false
        pretty(io, sp)
    end
    print(io, "]")
end
pretty(io::IO, p::BoundFetchFieldPattern) = print(io, p.input, ".", p.field_name)
pretty(io::IO, p::BoundFetchIndexPattern) = print(io, p.input, "[", p.index, "]")
pretty(io::IO, p::BoundFetchRangePattern) = print(io, p.input, "[", p.first_index, ":", p.from_end, "]")
pretty(io::IO, p::BoundFetchLengthPattern) = print(io, "length(", p.input, ")")

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
    for (v, t) in required_temps
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

#
# A state of the decision automaton (i.e. a point in the generated code),
# which is a set of partially matched cases.
#
mutable struct CodePoint
    # The state of the cases.  Impossible cases, which are designated by a
    # `false` `bound_pattern`, are removed from this array.  Cases are always
    # ordered by `case_number`.
    cases::ImmutableVector{CasePartialResult}

    # When the cases have been exhausted in the state machine, the cases in the tail
    # are then handled.  This offers a simple way to build the state machine as a
    # directed acyclic graph, for example to handle "or" patterns.
    tail::Union{Nothing, CodePoint}

    # A label to produce in the code at entry to the code where
    # this state is implemented, if one is needed.
    label::Union{Nothing, Symbol}

    # The selected action to take from this state: either
    # - Nothing, before it has been computed, or
    # - Case whose tests have all passed, or
    # - A bound pattern to perform and then move on to the next state, or
    # - An Expr to insert into the code when all else is exhausted
    #   (which throws MatchFailure or yields the programmer's result)
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
function name(code::CodePoint)
    if !isdefined(@__MODULE__, :baseref)
        global baseref = UInt(pointer_from_objref(code))
    end
    name = UInt(pointer_from_objref(code)) - baseref
    if code.label isa Nothing
        "CodePoint $name"
    else
        "CodePoint $name ($(code.label))"
    end
end
function pretty(io::IO, code::CodePoint)
    println(io, name(code))
    for case in code.cases
        print(io, "  ")
        pretty(io, case)
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
        println(io, "completed case ", action.case_number, " with value ", action.result_expression)
    elseif action isa BoundPattern
        print(io, "execute pattern ")
        pretty(io, action)
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

function dumpall(io::IO, root::CodePoint)
    all::Vector{CodePoint} = reachable_states(root)
    println(io)
    println(io, "State Machine:")
    for code in all
        pretty(io, code)
    end
    println(io, "end # of state machine")
end
#
# Print the entire state machine to `stdout`.  Useful for debugging the state
# machine and looking for optimization opportunities.
#
dumpall(root::CodePoint) = dumpall(stdout, root)

#
# Bind a case, producing a case partial result.
#
function bind_case(
    case_number::Int,
    location::LineNumberNode,
    case,
    state::BinderState)
    if !(@capture(case, pattern_ => result_))
        error("$(location.file):$(location.line): Unrecognized @match case syntax: `$case`.")
    end

    (bound_pattern, assigned) = bind_pattern!(
        location, pattern, state.input_variable, state, ImmutableDict{Symbol,Symbol}())
    return CasePartialResult(case_number, location, bound_pattern, assigned, result)
end

function handle_match2_cases(location::LineNumberNode, mod::Module, value, match)
    if !(match isa Expr) || match.head != :block
        error("$(location.file):$(location.line): Unrecognized @match block syntax: `$match`.")
    end

    input_variable::Symbol = gensym("input_value")
    state = BinderState(mod, input_variable)
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
    intern = Dict{CodePoint, CodePoint}()
    global baseref = UInt(pointer_from_objref(entry))
    while !isempty(work_queue)
        code = pop!(work_queue)
        if code.action isa Nothing
            set_next!(code, state, intern)
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

    dumpall(entry)
    return nothing


    error("not implemented")

    # result_variable = gensym("match_result")
    # code = Any[state.assertions..., :($input_variable = $(esc(value)))]
    # tail = :(throw(MatchFailure($input_variable)))
    # n = length(cases)
    # for (i, case) in enumerate(reverse(cases))
    #     eval = Expr(:block, case.location, esc(case.result_expression))
    #     result = Expr(:let, Expr(:block, assignments(case.assigned)...), eval)
    #     tail = Expr(i == n ? :if : :elseif, case.matched_expression, result, tail)
    # end
    # Expr(:block, state.assertions..., :($input_variable = $(esc(value))), tail)

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
function set_next!(code::CodePoint, state::BinderState, intern::Dict{CodePoint, CodePoint})
    @assert code.action isa Nothing
    @assert code.next isa Nothing

    action::Union{CasePartialResult, BoundPattern, Expr} = next_action(code, state)
    next::Union{Tuple{}, Tuple{CodePoint}, Tuple{CodePoint, CodePoint}} =
        make_next(code, action, state, intern)
    code.action = action
    code.next = next
    @assert !(code.next isa Nothing)
end

function make_next(
    code::CodePoint,
    action::Union{CasePartialResult, Expr},
    state::BinderState,
    intern::Dict{CodePoint, CodePoint})
    return ()
end
function make_next(
    code::CodePoint,
    action::BoundPattern,
    state::BinderState,
    intern::Dict{CodePoint, CodePoint})
    error("pattern cannot be the next action: $(typeof(action))")
end
function make_next(
    code::CodePoint,
    action::BoundFetchPattern,
    ::BinderState,
    intern::Dict{CodePoint, CodePoint})::CodePoint
    cases = map(case -> remove(action, case), code.cases)
    cases = filter(case -> !(case.pattern isa BoundFalsePattern), cases)
    succ = with_cases(code, cases)
    succ = get!(intern, succ, succ)
    return (succ,)
end
function remove(action::BoundFetchPattern, pattern::BoundPattern)
    pattern
end
function remove(action::BoundFetchPattern, pattern::BoundFetchPattern)
    return (action == pattern) ? BoundTruePattern(pattern.location, pattern.source) : pattern
end
function remove(action::BoundFetchPattern, pattern::BoundAndPattern)
    subpatterns = map(p -> remove(action, p), pattern.subpatterns)
    return BoundAndPattern(pattern.location, pattern.source, subpatterns)
end
function remove(action::BoundFetchPattern, pattern::BoundOrPattern)
    subpatterns = map(p -> remove(action, p), pattern.subpatterns)
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
    state::BinderState,
    intern::Dict{CodePoint, CodePoint})::Tuple{CodePoint, CodePoint}
    true_next = remove(action, TrueSense, code)
    false_next = remove(action, FalseSense, code)
    # TODO: Remove a common tail between the two successor states
    true_next = get!(intern, true_next, true_next)
    false_next = get!(intern, false_next, false_next)
    return (true_next, false_next)
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
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundPattern)
    error("not implemented: remove(::$(typeof(action)), ::$(typeof(sense)), ::$(typeof(pattern)))")
end
function remove(action::BoundTestPattern, sense::Sense, pattern::Union{BoundTruePattern, BoundFalsePattern, BoundFetchPattern})
    pattern
end
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundAndPattern)
    BoundAndPattern(pattern.location, pattern.source, map(p -> remove(action, sense, p), pattern.subpatterns))
end
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundOrPattern)
    BoundOrPattern(pattern.location, pattern.source, map(p -> remove(action, sense, p), pattern.subpatterns))
end
function remove(action::BoundTestPattern, sense::Sense, pattern::BoundTestPattern)
    if (action == pattern)
        BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
    else
        pattern
    end
end
# function remove(action::BoundRelationalTestPattern, sense::Sense, pattern::BoundRelationalTestPattern)
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
# function remove(action::BoundEqualValueTestPattern, sense::Sense, pattern::BoundEqualValueTestPattern)
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         error("not implemented")
#     else
#         pattern
#     end
# end
# function remove(action::BoundTypeTestPattern, sense::Sense, pattern::BoundTypeTestPattern)
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         error("not implemented")
#     else
#         pattern
#     end
# end
# function remove(action::BoundEqualValueTestPattern, sense::Sense, pattern::BoundRelationalTestPattern)
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         error("not implemented")
#     else
#         pattern
#     end
# end
# function remove(action::BoundRelationalTestPattern, sense::Sense, pattern::BoundEqualValueTestPattern)
#     if (action == pattern)
#         BoundBoolPattern(pattern.location, pattern.source, typeof(sense).parameters[1])
#     else if action.input == pattern.input
#         error("not implemented")
#     else
#         pattern
#     end
# end
