#
# A data structure representing information about a match case in one
# node of the decision automaton for a @match block statement.
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
    # In this node of the automaton, some operations may have already been done,
    # and they are removed from the bound pattern.  When the bound pattern is
    # simply `true`, it has matched.
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
function pretty(io::IO, case::CasePartialResult, binder::BinderContext)
    print(io, case.case_number, ": ")
    pretty(io, case.pattern, binder)
    print(io, " => ")
    pretty(io, case.result_expression)
    println(io)
end

abstract type AbstractAutomatonNode end

#
# A node of the decision automaton (i.e. a point in the generated code),
# which is represented a set of partially matched cases.
#
mutable struct AutomatonNode <: AbstractAutomatonNode
    # The status of the cases.  Impossible cases, which are designated by a
    # `false` `bound_pattern`, are removed from this array.  Cases are always
    # ordered by `case_number`.
    @_const cases::ImmutableVector{CasePartialResult}

    # The selected action to take from this node: either
    # - Nothing, before it has been computed, or
    # - Case whose tests have all passed, or
    # - A bound pattern to perform and then move on to the next node, or
    # - An Expr to insert into the code when all else is exhausted
    #   (which throws MatchFailure)
    action::Union{Nothing, CasePartialResult, BoundPattern, Expr}

    # The next node(s):
    # - Nothing before being computed
    # - Tuple{} if the action is a case which was matched or a MatchFailure
    # - Tuple{AutomatonNode} if the action was a fetch pattern. It designates
    #   the node for code that follows the fetch.
    # - Tuple{AutomatonNode, AutomatonNode} if the action is a test.  These are the nodes
    #   to go to if the result of the test is true ([1]) or false ([2]).
    next::Union{Nothing, Tuple{}, Tuple{AutomatonNode}, Tuple{AutomatonNode, AutomatonNode}}

    @_const _cached_hash::UInt64

    function AutomatonNode(cases::Vector{CasePartialResult})
        cases = filter(case -> !(case.pattern isa BoundFalsePattern), cases)
        for i in eachindex(cases)
            if is_irrefutable(cases[i].pattern)
                cases = cases[1:i]
                break
            end
        end
        new(ImmutableVector(cases), nothing, nothing, hash(cases, 0xc98a9a23c2d4d915))
    end
end
Base.hash(case::AutomatonNode, h::UInt64) = hash(case._cached_hash, h)
function Base.:(==)(a::AutomatonNode, b::AutomatonNode)
    a === b ||
        a._cached_hash == b._cached_hash &&
        isequal(a.cases, b.cases)
end
function name(node::T, id::IdDict{T, Int}) where { T <: AbstractAutomatonNode }
    "Node $(id[node])"
end
function successors(c::T)::Vector{T} where { T <: AbstractAutomatonNode }
    @assert !(c.next isa Nothing)
    collect(c.next)
end
function reachable_nodes(root::T)::Vector{T} where { T <: AbstractAutomatonNode }
    topological_sort(successors, [root])
end

#
# Support for pretty-printing
#
function dumpall(io::IO, all::Vector{T}, binder::BinderContext, long::Bool) where { T <: AbstractAutomatonNode }
    # Make a map from each node to its index
    id = IdDict{T, Int}(map(((i,s),) -> s => i, enumerate(all))...)
    print(io, "Decision Automaton: ($(length(all)) nodes) input ")
    pretty(io, binder.input_variable)
    println(io)
    for node in all
        pretty(io, node, binder, id, long)
        println(io)
    end
    println(io, "end # of automaton")
    length(all)
end

# Pretty-print either an AutomatonNode or a DeduplicatedAutomatonNode
function pretty(
    io::IO,
    node::T,
    binder::BinderContext,
    id::IdDict{T, Int},
    long::Bool = true) where { T <: AbstractAutomatonNode }
    print(io, name(node, id))
    if long && hasfield(T, :cases)
        println(io)
        for case in node.cases
            print(io, "  ")
            pretty(io, case, binder)
        end
    end
    action = node.action
    long && print(io, "   ")
    if action isa CasePartialResult
        print(io, " MATCH ", action.case_number, " with value ")
        pretty(io, action.result_expression)
    elseif action isa BoundPattern
        if action isa BoundTestPattern
            print(io, " TEST ")
        elseif action isa BoundFetchPattern
            print(io, " FETCH ")
        else
            print(io, " UNKNOWN ")
        end
        pretty(io, action, binder)
    elseif action isa Expr
        print(io, " FAIL ")
        pretty(io, action)
    else
        error(" UNKNOWN ")
    end
    next = node.next
    if next isa Tuple{T}
        fall_through = id[next[1]] == id[node] + 1
        if long || !fall_through
            long && print(io, "\n   ")
            print(io, " NEXT: $(name(next[1], id))")
            if id[next[1]] == id[node] + 1
                print(io, " (fall through)")
            end
        end
    elseif next isa Tuple{T, T}
        fall_through = id[next[1]] == id[node] + 1
        if long || !fall_through
            long && print(io, "\n   ")
            print(io, " THEN: $(name(next[1], id))")
            if id[next[1]] == id[node] + 1
                print(io, " (fall through)")
            end
            long && println(io)
        end
        long && print(io, "   ")
        print(io, " ELSE: $(name(next[2], id))")
    elseif next isa Tuple{}
    else
        error(" UNKNOWN ")
    end
end

# We merge nodes with identical behavior, bottom-up, to minimize the size of
# the decision automaton.  We define `hash` and `==` to take account of only what matters.
# Specifically, we ignore the `cases::ImmutableVector{CasePartialResult}` of `AutomatonNode`.
mutable struct DeduplicatedAutomatonNode <: AbstractAutomatonNode
    # A label to produce in the code at entry to the code where
    # this node is implemented, if one is needed.  This is not produced
    # when this struct is created, but later during code generation.
    label::Union{Nothing, Symbol}

    # The selected action to take from this node: either
    # - Case whose tests have all passed, or
    # - A bound pattern to perform and then move on to the next node, or
    # - An Expr to insert into the code when all else is exhausted
    #   (which throws MatchFailure)
    @_const action::Union{CasePartialResult, BoundPattern, Expr}

    # The next code point(s):
    # - Tuple{} if the action is a case which was matched or a MatchFailure
    # - Tuple{DeduplicatedAutomatonNode} if the action was a fetch pattern. It designates
    #   the note to go to after the fetch.
    # - Tuple{DeduplicatedAutomatonNode, DeduplicatedAutomatonNode} if the action is a
    #   test.  These are the nodes to go to if the result of the test is true ([1]) or
    #   false ([2]).
    @_const next::Union{Tuple{}, Tuple{DeduplicatedAutomatonNode}, Tuple{DeduplicatedAutomatonNode, DeduplicatedAutomatonNode}}

    @_const _cached_hash::UInt64
    function DeduplicatedAutomatonNode(action, next)
        action isa CasePartialResult && @assert action.pattern isa BoundTruePattern
        new(nothing, action, next, hash((action, next)))
    end
end
Base.hash(node::DeduplicatedAutomatonNode, h::UInt64) = hash(node._cached_hash, h)
Base.hash(node::DeduplicatedAutomatonNode) = node._cached_hash
function Base.:(==)(a::DeduplicatedAutomatonNode, b::DeduplicatedAutomatonNode)
    a === b ||
        a._cached_hash == b._cached_hash &&
        isequal(a.action, b.action) &&
        isequal(a.next, b.next)
end
function ensure_label!(node::DeduplicatedAutomatonNode, binder::BinderContext)
    if node.label isa Nothing
        node.label = gensym("label", binder)
    end
end

#
# Deduplicate a node, given the deduplications of the downstream nodes.
# Has the side-effect of adding a mapping to the dict.
#
function dedup!(
    dict::Dict{DeduplicatedAutomatonNode, DeduplicatedAutomatonNode},
    node::AutomatonNode,
    binder::BinderContext)
    next = if node.next isa Tuple{}
        node.next
    elseif node.next isa Tuple{AutomatonNode}
        (dedup!(dict, node.next[1], binder),)
    elseif node.next isa Tuple{AutomatonNode, AutomatonNode}
        t = dedup!(dict, node.next[1], binder)
        f = dedup!(dict, node.next[2], binder)
        (t, f)
    else
        error("Unknown next type: $(node.next)")
    end
    key = DeduplicatedAutomatonNode(node.action, next)
    result = get!(dict, key, key)
    result
end

#
# Deduplicate the decision automaton by collapsing behaviorally identical nodes.
#
function deduplicate_automaton(entry::AutomatonNode, binder::BinderContext)
    dedup_map = Dict{DeduplicatedAutomatonNode, DeduplicatedAutomatonNode}()
    result = Vector{DeduplicatedAutomatonNode}()
    top_down_nodes = reachable_nodes(entry)
    for e in Iterators.reverse(top_down_nodes)
        _ = dedup!(dedup_map, e, binder)
    end
    new_entry = dedup!(dedup_map, entry, binder)
    return reachable_nodes(new_entry)
end
