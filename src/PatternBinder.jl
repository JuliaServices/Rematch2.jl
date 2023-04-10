
# We have a node for each pattern form.  Some syntactic pattern forms are test_broken
# up into more primitive forms.  For example, the pattern `s::String` is represented as
# an `AndPattern` that combines a `TypePattern` with a `BindVariablePattern`.
abstract type Pattern end

# A pattern like `1`, `$(expression)`, or `x` where `x` is already bound.
# Note that for a pattern variable `x` that is previously bound, `x` means
# the same thing as `$x` or `$(x)`.  We test a constant pattern by applying
# `isequal(input_value, pattern.value)`
struct ConstantPattern <: Pattern
    location::LineNumberNode
    source::Any
    value
end

# A pattern that compares the input, which must be an Integer, using a Relational
# operator, to a given value.  Used to ensure that list patterns match against a
# list of sufficient length.
struct RelationalPattern <: Pattern
    location::LineNumberNode
    source::Any
    relation::Symbol # one of `:<`, `:<=`, `:>`, `:>=`
    value::Int
end

# A pattern like `_` which matches but does not save the input
struct WildcardPattern <: Pattern
    location::LineNumberNode
    source::Any
end

# A pattern like `x`, which matches and binds the input to that variable
struct BindVariablePattern <: Pattern
    location::LineNumberNode
    source::Any
    variable::Symbol
end

# A pattern like ::Type which matches if the type matches.
struct TypePattern <: Pattern
    location::LineNumberNode
    source::Any
    variable::Symbol
end

# A pattern that matches if either disjunct matches
struct OrPattern <: Pattern
    location::LineNumberNode
    source::Any
    pattern1::Pattern
    pattern2::Pattern
end

# A pattern that matches if both conjuncts match
struct AndPattern <: Pattern
    location::LineNumberNode
    source::Any
    pattern1::Pattern
    pattern2::Pattern
end

# A pattern that matches if the expression, when evaluated, yields true
struct WherePattern <: Pattern
    location::LineNumberNode
    source::Any
    expression
end

# Patterns which fetch intermediate values so they may be reused later without
# being recomputed.  Each one is treated like a pattern that is implicitly
# true (matches everything), but has the side-effect of assigning a computed
# value to a temporary variable.
# abstract type Fetch <: Pattern

# Fetch a field of the input into into a fresh temporary synthetic variable.
# Used to decompose patterns that match subfields.  Treated as always "true"
# for matching purposes, except it has the side effect of producing a temporary
# variable that can be used for further tests.  That temporary may be reused across
# patterns when that makes sense.
struct FetchFieldPattern <: Pattern
    location::LineNumberNode
    source::Any
    field_name::Symbol
end

# For the purposes of whether or not two fetches are the same: if they are fetching
# the same field name (from the same input), then yes.
Base.hash(a::FetchFieldPattern, h::UInt64) = hash((a.field_name, 0x585ebcb0d59e10a1), h)
function Base.:(==)(a::FetchFieldPattern, b::FetchFieldPattern)
    a.field_name == b.field_name
end

# Fetch a value at a given index of the input into a temporary.  See `FetchFieldPattern`
# for the general idea of how these are used.  Negative indices index from the end of
# the input.
struct FetchIndexPattern <: Pattern
    location::LineNumberNode
    source::Any
    index::Int
end
Base.hash(a::FetchIndexPattern, h::UInt64) = hash((a.index, 0x17a20105628b009c), h)
function Base.:(==)(a::FetchIndexPattern, b::FetchIndexPattern)
    a.index == b.index
end

# Compute the length of the input (tuple or array)
struct FetchLengthPattern <: Pattern
    location::LineNumberNode
    source::Any
end
Base.hash(::FetchLengthPattern, h::UInt64) = hash(0x9f4462978fc773a8, h)
Base.:(==)(a::FetchLengthPattern, b::FetchLengthPattern) = true

const FetchPattern = Union{FetchFieldPattern, FetchIndexPattern, FetchLengthPattern}

# Persistent data that we use across different patterns, to ensure the same computations
# is always represented by the same synthetic variables.
struct BinderState
    # The bindings for user named pattern variables.  The key is the user variable and the
    # value is the generated symbol for the variable into which it shall be computed.
    pattern_variables::Dict{Symbol,Symbol}

    # The bindings to be used for each intermediate computations.
    assignments::Dict{Tuple{Symbol,FetchPattern},Symbol}

    # The type of synthetic intermediate variables
    types::Dict{Symbol,Type}

    # A mapping from an interpolated expression (with syntetic variables
    # substituted in) to a temporary that may be used to hold its value.
    # A simple variable will be represented by a block containing its
    # reference.  These interpolations are also used for each top-level
    # conjunct of a where clause.
    interpolations::Dict{Any,Symbol}

    function BinderState()
        new(Dict{Symbol,Symbol}(),
            Dict{(Symbol,FetchPattern),Symbol}(),
            Dict{Symbol,Type}(),
            Dict{Any,Symbol}())
    end
end

# A bound pattern, by which we keep track of which synthetic variables are bound.
struct BoundPattern
    pattern::Pattern
    bindings_after::Set{Symbol}
end

function bind_pattern!(
    location::LineNumberNode,
    source::Any,
    input::Symbol,
    state::BinderState,
    assigned::Set{Symbol})::BoundPattern
    pattern::Pattern
    if (source == :_)
        pattern = WildcardPattern(location, source)
    else
        error()
    end
    return BoundPattern(pattern, assigned)
end

function bind_pattern(
    location::LineNumberNode,
    source::Any,
    input::Any)::(pattern::BoundPattern, pattern_variables::Dict{Symbol,Symbol})
    state = BinderState()
    assigned = Set{Symbol}()

    # Create a temporary symbol to hold the input
    input_variable = gensym("input_value#$location")
    push!(assigned, input_vatiable)

    pattern = bind_pattern!(location, source, input_variable, state, assigned)
    return (pattern, state.pattern_variables)
end
