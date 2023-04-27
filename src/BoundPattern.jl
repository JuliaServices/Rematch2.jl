
# We have a node for each pattern form.  Some syntactic pattern forms are test_broken
# up into more primitive forms.  For example, the pattern `s::String` is represented as
# an `AndPattern` that combines a `TypePattern` with a `BindVariablePattern`.
abstract type BoundPattern end

# A pattern that always matches
struct TrueBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
end
Base.hash(::TrueBoundPattern, h::UInt64) = hash(0x8cc17f34ef3bbb1d, h)
Base.:(==)(a::TrueBoundPattern, b::TrueBoundPattern) = true

# A pattern that never matches
struct FalseBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
end
Base.hash(::FalseBoundPattern, h::UInt64) = hash(0xeb817c7d6beb3bda, h)
Base.:(==)(a::FalseBoundPattern, b::FalseBoundPattern) = true

# A pattern like `1`, `$(expression)`, or `x` where `x` is already bound.
# Note that for a pattern variable `x` that is previously bound, `x` means
# the same thing as `$x` or `$(x)`.  We test a constant pattern by applying
# `isequal(input_value, pattern.value)`
struct EqualValueBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    value::Any  # the value that the input should be compared to using `isequal`
    assigned::ImmutableDict{Symbol,Symbol}
end
Base.hash(a::EqualValueBoundPattern, h::UInt64) =
    hash((a.input, a.value, a.assigned, 0x7e92a644c831493f), h)
Base.:(==)(a::EqualValueBoundPattern, b::EqualValueBoundPattern) =
    a.input == b.input && a.value == b.value && a.assigned == b.assigned

# A pattern that compares the input, which must be an Integer, using a Relational
# operator, to a given value.  Used to ensure that list patterns match against a
# list of sufficient length.  The input is on the left and the value is on the
# right of whatever relation is used.  Currently only `>=` is supported.
struct RelationalBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    relation::Symbol # one of `:<`, `:<=`, `:>`, `:>=`
    value::Int
end
Base.hash(a::RelationalBoundPattern, h::UInt64) =
    hash((a.input, a.relation, a.value, 0xbfe66949d262f0e0), h)
Base.:(==)(a::RelationalBoundPattern, b::RelationalBoundPattern) =
    a.input == b.input && a.relation == b.relation && a.value == b.value

# A pattern that evaluates the given boolean expression given the assignments.
struct WhereBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    assigned::ImmutableDict{Symbol,Symbol}
end
Base.hash(a::WhereBoundPattern, h::UInt64) =
    hash((a.source, a.assigned, 0x868a8076acbe0e12), h)
Base.:(==)(a::WhereBoundPattern, b::WhereBoundPattern) =
    a.source == b.source && a.assigned == b.assigned

# A pattern like ::Type which matches if the type matches.
struct TypeBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    type::Type # typically the type that source binds to
end
Base.hash(a::TypeBoundPattern, h::UInt64) =
    hash((a.input, a.relation, a.value, 0x92b4a01b9f8cb47b), h)
Base.:(==)(a::TypeBoundPattern, b::TypeBoundPattern) =
    a.input == b.input && a.type == b.type

# A pattern that matches if any disjunct matches
struct OrBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    subpatterns::Vector{BoundPattern}
    function OrBoundPattern(
        location::LineNumberNode,
        source::Any,
        subpatterns::Vector{BoundPattern})
        # We gather together (flatten) any nested and-patterns
        gathered_subpatterns = mapreduce(
            p -> if p isa OrBoundPattern
                p.subpatterns
            elseif p isa FalseBoundPattern
                []
            else
                [p]
            end,
            vcat,
            subpatterns)
        if any(p -> p isa TrueBoundPattern, gathered_subpatterns)
            return TrueBoundPattern(location, source)
        elseif isempty(gathered_subpatterns)
            return FalseBoundPattern(location, source)
        elseif length(gathered_subpatterns) == 1
            return gathered_subpatterns[1]
        else
            return new(location, source, gathered_subpatterns)
        end
    end
end

# A pattern that matches if all conjuncts match
struct AndBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    subpatterns::Vector{BoundPattern}
    function AndBoundPattern(
        location::LineNumberNode,
        source::Any,
        subpatterns::Vector{BoundPattern})
        # We gather together (flatten) any nested and-patterns
        gathered_subpatterns = mapreduce(
            p -> if p isa AndBoundPattern
                p.subpatterns
            elseif p isa TrueBoundPattern
                []
            else
                [p]
            end,
            vcat,
            subpatterns)
        if any(p -> p isa FalseBoundPattern, gathered_subpatterns)
            return FalseBoundPattern(location, source)
        elseif isempty(gathered_subpatterns)
            return TrueBoundPattern(location, source)
        elseif length(gathered_subpatterns) == 1
            return gathered_subpatterns[1]
        else
            return new(location, source, gathered_subpatterns)
        end
    end
end

# Fetch a field of the input into into a fresh temporary synthetic variable.
# Used to decompose patterns that match subfields.  Treated as always "true"
# for matching purposes, except it has the side effect of producing a temporary
# variable that can be used for further tests.  That temporary may be reused across
# patterns when that makes sense.
struct FetchFieldBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    field_name::Symbol
end

# For the purposes of whether or not two fetches are the same: if they are fetching
# the same field name (from the same input), then yes.
Base.hash(a::FetchFieldBoundPattern, h::UInt64) =
    hash((a.input, a.field_name, 0x0c5266ab2b5ed7f1), h)
function Base.:(==)(a::FetchFieldBoundPattern, b::FetchFieldBoundPattern)
    a.input == b.input && a.field_name == b.field_name
end

# Fetch a value at a given index of the input into a temporary.  See `FetchFieldBoundPattern`
# for the general idea of how these are used.  Negative indices index from the end of
# the input; `index==-1` accesses the last element.
struct FetchIndexBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    index::Int   # index value.  If negative, it is from the end.  `-1` accesses the last element
end
Base.hash(a::FetchIndexBoundPattern, h::UInt64) =
    hash((a.input, a.index, 0x820a6d07cc13ac86), h)
function Base.:(==)(a::FetchIndexBoundPattern, b::FetchIndexBoundPattern)
    a.input == b.input && a.index == b.index
end

# Fetch a subsequence at a given range of the input into a temporary.
struct FetchRangeBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    first_index::Int # first index to include
    from_end::Int    # distance from the end for the last included index; 0 to include the last element
end
Base.hash(a::FetchRangeBoundPattern, h::UInt64) =
    hash((a.input, a.first_index, a.from_end, 0x7aea7756428a1646), h)
function Base.:(==)(a::FetchRangeBoundPattern, b::FetchRangeBoundPattern)
    a.input == b.input && a.first_index == b.first_index && a.from_end == b.from_end
end

# Compute the length of the input (tuple or array)
struct FetchLengthBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol
end
Base.hash(a::FetchLengthBoundPattern, h::UInt64) =
    hash((a.input, 0xa7167fae5a24c457), h)
function Base.:(==)(a::FetchLengthBoundPattern, b::FetchLengthBoundPattern)
    a.input == b.input
end

# Preserve the current binding of the user variable into a fixed variable-specific temp.
# Used to force the binding on both sides of an or-pattern to be the same.  The temp used
# for this depends only on the variable.
struct FetchBindingBoundPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    input::Symbol    # previous binding of the variable
    variable::Symbol # user variable to be preserved
end
Base.hash(a::FetchBindingBoundPattern, h::UInt64) =
    hash((a.input, a.variable, 0x53f0f6a137a891d8), h)
function Base.:(==)(a::FetchBindingBoundPattern, b::FetchBindingBoundPattern)
    a.input == b.input && a.variable == b.variable
end

# Patterns which fetch intermediate values so they may be reused later without
# being recomputed.  Each one has the side-effect of assigning a computed
# value to a temporary variable.
const FetchPattern = Union{
    FetchFieldBoundPattern,
    FetchLengthBoundPattern,
    FetchIndexBoundPattern,
    FetchRangeBoundPattern,
    FetchBindingBoundPattern}
