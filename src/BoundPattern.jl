
# const Assigned = ImmutableDict{Symbol, Symbol}

# We have a node for each pattern form.  Some syntactic pattern forms are broken
# up into more primitive forms.  For example, the pattern `s::String` is represented as
# an `AndPattern` that combines a `TypePattern` (for String) with a
# `BindVariablePattern` (to bind s).
abstract type BoundPattern end

# Patterns which fetch intermediate values so they may be reused later without
# being recomputed.  Each one has the side-effect of assigning a computed
# value to a temporary variable.
abstract type BoundFetchPattern <: BoundPattern end

# Patterns which test some boolean condition.
abstract type BoundTestPattern <: BoundPattern end

# A pattern that always matches
struct BoundTruePattern <: BoundPattern
    location::LineNumberNode
    source::Any
end
Base.hash(::BoundTruePattern, h::UInt64) = hash(0x8cc17f34ef3bbb1d, h)
Base.:(==)(a::BoundTruePattern, b::BoundTruePattern) = true

# A pattern that never matches
struct BoundFalsePattern <: BoundPattern
    location::LineNumberNode
    source::Any
end
Base.hash(::BoundFalsePattern, h::UInt64) = hash(0xeb817c7d6beb3bda, h)
Base.:(==)(a::BoundFalsePattern, b::BoundFalsePattern) = true

function BoundBoolPattern(location::LineNumberNode, source::Any, b::Bool)
    b ? BoundTruePattern(location, source) : BoundFalsePattern(location, source)
end

# A pattern like `1`, `$(expression)`, or `x` where `x` is already bound.
# Note that for a pattern variable `x` that is previously bound, `x` means
# the same thing as `$x` or `$(x)`.  We test a constant pattern by applying
# `isequal(input_value, pattern.value)`
struct BoundEqualValueTestPattern <: BoundTestPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    value::Any  # the value that the input should be compared to using `isequal`
    assigned::ImmutableDict{Symbol, Symbol}
end
function Base.hash(a::BoundEqualValueTestPattern, h::UInt64)
    hash((a.input, a.value, a.assigned, 0x7e92a644c831493f), h)
end
function Base.:(==)(a::BoundEqualValueTestPattern, b::BoundEqualValueTestPattern)
    a.input == b.input && isequal(a.value, b.value) && a.assigned == b.assigned
end

# A pattern that compares the input, which must be an Integer, using a Relational
# operator, to a given value.  Used to ensure that list patterns match against a
# list of sufficient length.  The input is on the left and the value is on the
# right of whatever relation is used.  Currently only `>=` is supported.
struct BoundRelationalTestPattern <: BoundTestPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    relation::Symbol # one of `:<`, `:<=`, `:>`, `:>=`
    value::Int
end
function Base.hash(a::BoundRelationalTestPattern, h::UInt64)
    hash((a.input, a.relation, a.value, 0xbfe66949d262f0e0), h)
end
function Base.:(==)(a::BoundRelationalTestPattern, b::BoundRelationalTestPattern)
    a.input == b.input && a.relation == b.relation && a.value == b.value
end

# A pattern that simply checks the given boolean variable
struct BoundWhereTestPattern <: BoundTestPattern
    location::LineNumberNode
    source::Any
    input::Symbol # variable holding the evaluated where clause
    inverted::Bool # If the sense of the test is inverted
end
function Base.hash(a::BoundWhereTestPattern, h::UInt64)
    hash((a.input, a.inverted, 0x868a8076acbe0e12), h)
end
function Base.:(==)(a::BoundWhereTestPattern, b::BoundWhereTestPattern)
    a.input == b.input && a.inverted == b.inverted
end
function pretty(io::IO, p::BoundWhereTestPattern)
    print(io, "where ")
    p.inverted && print(io, "!")
    pretty(io, p.input)
end

# A pattern like ::Type which matches if the type matches.
struct BoundTypeTestPattern <: BoundTestPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    type::Type # typically the type that source binds to
end
function Base.hash(a::BoundTypeTestPattern, h::UInt64)
    hash((a.input, a.type, 0x92b4a01b9f8cb47b), h)
end
function Base.:(==)(a::BoundTypeTestPattern, b::BoundTypeTestPattern)
    a.input == b.input && a.type == b.type
end

# A pattern that matches if any disjunct matches
struct BoundOrPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    subpatterns::ImmutableVector{BoundPattern}
    _cached_hash::UInt64
    function BoundOrPattern(
        location::LineNumberNode,
        source::Any,
        subpatterns::Vector{BoundPattern})
        # We gather together (flatten) any nested and-patterns
        gathered_subpatterns = mapreduce(
            p -> if p isa BoundOrPattern
                p.subpatterns
            elseif p isa BoundFalsePattern
                BoundPattern[]
            else
                BoundPattern[p]
            end,
            vcat,
            subpatterns)
        if any(p -> p isa BoundTruePattern, gathered_subpatterns)
            return BoundTruePattern(location, source)
        elseif isempty(gathered_subpatterns)
            return BoundFalsePattern(location, source)
        elseif length(gathered_subpatterns) == 1
            return gathered_subpatterns[1]
        else
            return new(location, source, gathered_subpatterns,
                hash(subpatterns, 0x82d8dc51bf845b12))
        end
    end
end
Base.hash(a::BoundOrPattern, h::UInt64) = hash(a._cached_hash, h)
Base.hash(a::BoundOrPattern) = a._cached_hash
function Base.:(==)(a::BoundOrPattern, b::BoundOrPattern)
    a._cached_hash == b._cached_hash && a.subpatterns == b.subpatterns
end

# A pattern that matches if all conjuncts match
struct BoundAndPattern <: BoundPattern
    location::LineNumberNode
    source::Any
    subpatterns::Vector{BoundPattern}
    _cached_hash::UInt64
    function BoundAndPattern(
        location::LineNumberNode,
        source::Any,
        subpatterns::Vector{BoundPattern})
        # We gather together (flatten) any nested and-patterns
        gathered_subpatterns = mapreduce(
            p -> if p isa BoundAndPattern
                p.subpatterns
            elseif p isa BoundTruePattern
                BoundPattern[]
            else
                BoundPattern[p]
            end,
            vcat,
            subpatterns)
        if any(p -> p isa BoundFalsePattern, gathered_subpatterns)
            return BoundFalsePattern(location, source)
        elseif isempty(gathered_subpatterns)
            return BoundTruePattern(location, source)
        elseif length(gathered_subpatterns) == 1
            return gathered_subpatterns[1]
        else
            return new(location, source, gathered_subpatterns,
                hash(subpatterns, 0x9b7f2a204d994a1a))
        end
    end
end
Base.hash(a::BoundAndPattern, h::UInt64) = hash(a._cached_hash, h)
Base.hash(a::BoundAndPattern) = a._cached_hash
function Base.:(==)(a::BoundAndPattern, b::BoundAndPattern)
    a._cached_hash == b._cached_hash && a.subpatterns == b.subpatterns
end

# Fetch a field of the input into into a fresh temporary synthetic variable.
# Used to decompose patterns that match subfields.  Treated as always "true"
# for matching purposes, except it has the side effect of producing a temporary
# variable that can be used for further tests.  That temporary may be reused across
# patterns when that makes sense.
struct BoundFetchFieldPattern <: BoundFetchPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    field_name::Symbol
end
# For the purposes of whether or not two fetches are the same: if they are fetching
# the same field name (from the same input), then yes.
function Base.hash(a::BoundFetchFieldPattern, h::UInt64)
    hash((a.input, a.field_name, 0x0c5266ab2b5ed7f1), h)
end
function Base.:(==)(a::BoundFetchFieldPattern, b::BoundFetchFieldPattern)
    a.input == b.input && a.field_name == b.field_name
end

# Fetch a value at a given index of the input into a temporary.  See `BoundFetchFieldPattern`
# for the general idea of how these are used.  Negative indices index from the end of
# the input; `index==-1` accesses the last element.
struct BoundFetchIndexPattern <: BoundFetchPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    # index value.  If negative, it is from the end.  `-1` accesses the last element
    index::Int
end
function Base.hash(a::BoundFetchIndexPattern, h::UInt64)
    hash((a.input, a.index, 0x820a6d07cc13ac86), h)
end
function Base.:(==)(a::BoundFetchIndexPattern, b::BoundFetchIndexPattern)
    a.input == b.input && a.index == b.index
end

# Fetch a subsequence at a given range of the input into a temporary.
struct BoundFetchRangePattern <: BoundFetchPattern
    location::LineNumberNode
    source::Any
    input::Symbol
    first_index::Int # first index to include
    from_end::Int    # distance from the end for the last included index; 0 to include the last element
end
function Base.hash(a::BoundFetchRangePattern, h::UInt64)
    hash((a.input, a.first_index, a.from_end, 0x7aea7756428a1646), h)
end
function Base.:(==)(a::BoundFetchRangePattern, b::BoundFetchRangePattern)
    a.input == b.input && a.first_index == b.first_index && a.from_end == b.from_end
end

# Compute the length of the input (tuple or array)
struct BoundFetchLengthPattern <: BoundFetchPattern
    location::LineNumberNode
    source::Any
    input::Symbol
end
function Base.hash(a::BoundFetchLengthPattern, h::UInt64)
    hash((a.input, 0xa7167fae5a24c457), h)
end
function Base.:(==)(a::BoundFetchLengthPattern, b::BoundFetchLengthPattern)
    a.input == b.input
end

# Preserve the value of the expression into a temp.
# Used (1) to force the binding on both sides of an or-pattern to be the same, and
# (2) to load the value of a `where` clause.
# The stored `value` has had substitutions (recorded in `assigned`) applied by the caller,
# so that semantically equivalent values might be syntactically equivalent.  The key
# is used (1) to select a name for the temp, and (2) to force distinct temps to be used
# at join points (when any of the bindings in `assigned` are phi merge points)
struct BoundFetchExpressionPattern <: BoundFetchPattern
    location::LineNumberNode
    source::Any
    value::Any    # value to be  preserved, e.g. previous binding of a variable
    assigned::ImmutableDict{Symbol, Symbol}
    key::Union{Nothing, Symbol}   # key to identify the temp, or user variable to be preserved
end
function BoundFetchExpressionPattern(
    location::LineNumberNode,
    source::Any,
    value::Any,
    assigned::ImmutableDict{Symbol, Symbol})
    key = any(p -> is_phi(p.second), assigned) ? gensym("where") : nothing
    BoundFetchExpressionPattern(location, source, value, assigned, key)
end
function Base.hash(a::BoundFetchExpressionPattern, h::UInt64)
    hash((a.value, a.key, 0x53f0f6a137a891d8), h)
end
function Base.:(==)(a::BoundFetchExpressionPattern, b::BoundFetchExpressionPattern)
    a.value == b.value && a.key == b.key
end
function pretty(io::IO, p::BoundFetchExpressionPattern)
    pretty(io, p.value)
end

#
# Pattern properties
#

# Pattern might not be true
is_refutable(pattern::Union{BoundFetchPattern, BoundTruePattern}) = false
is_refutable(pattern::Union{BoundTestPattern, BoundFalsePattern}) = true
is_refutable(pattern::BoundAndPattern) = any(is_refutable, pattern.subpatterns)
is_refutable(pattern::BoundOrPattern) = all(is_refutable, pattern.subpatterns)

# Pattern is definitely true
is_irrefutable(pattern::BoundPattern) = !is_refutable(pattern)
