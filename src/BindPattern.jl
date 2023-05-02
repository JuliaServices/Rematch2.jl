
# Persistent data that we use across different patterns, to ensure the same computations
# are always represented by the same synthetic variables.  We use this during lowering
# and also during code generation, since it holds some of the state required during code
# generation (such as assertions and assignments)
struct BinderState
    # The module containing the pattern, in which types appearing in the
    # pattern should be bound.
    mod::Module

    # The variable that contains the original input.
    input_variable::Symbol

    # The bindings to be used for each intermediate computations.  This maps from the
    # computation producing the value to the symbol for the temp holding that value.
    assignments::Dict{BoundFetchPattern,Symbol}

    # The set of type syntax forms that have asserted bindings in assertions
    asserted_types::Vector{Any}

    # Assertions that should be executed at runtime before the matching code.
    assertions::Vector{Any}

    function BinderState(mod::Module, input_variable::Symbol)
        new(
            mod,
            input_variable,
            Dict{BoundFetchPattern,Symbol}(),
            Vector{Pair{LineNumberNode, String}}(),
            Vector{Any}()
            )
    end
end

const saved_prefix = "saved#"
function get_temp(state::BinderState, p::BoundFetchPattern)
    get!(state.assignments, p) do; gensym(); end
end
get_temp(state::BinderState, p::BoundFetchBindingPattern) = get_temp(p.variable)
get_temp(p::Symbol) = Symbol(saved_prefix, p)

# We restrict the struct pattern to require something that looks like
# a type name before the open paren.  This improves the diagnostics
# for error cases like `(a + b)`, which produces an analogous Expr node
# but with `+` as the operator.
is_possible_type_name(t) = false
is_possible_type_name(t::Symbol) = Base.isidentifier(t)
function is_possible_type_name(t::Expr)
    t.head == :. &&
        is_possible_type_name(t.args[1]) &&
        t.args[2] isa QuoteNode &&
        is_possible_type_name(t.args[2].value)
end

function bind_pattern!(
    location::LineNumberNode,
    source::Any,
    input::Symbol,
    state::BinderState,
    assigned::ImmutableDict{Symbol,Symbol})

    if (source == :_)
        # wildcard pattern
        pattern = BoundTruePattern(location, source)

    elseif (!(source isa Expr || source isa Symbol) ||
        @capture(source, _quote_macrocall) ||
        @capture(source, Symbol(_)) # questionable
        )
        # a constant
        pattern = BoundEqualValueTestPattern(
            location, source, input, esc(source), ImmutableDict{Symbol, Symbol}())

    elseif (source isa Expr && source.head == :$)
        # an interpolation
        interpolation = source.args[1]
        pattern = BoundEqualValueTestPattern(
            location, interpolation, input, esc(interpolation), assigned)

    elseif @capture(source, varsymbol_Symbol)
        # variable pattern (just a symbol)
        if haskey(assigned, varsymbol)
            # previously introduced variable.  Get the symbol holding its value
            var_value = assigned[varsymbol]
            pattern = BoundEqualValueTestPattern(
                location, source, input, var_value, ImmutableDict{Symbol, Symbol}())
        else
            # this patterns assigns the variable.
            assigned = ImmutableDict(assigned, varsymbol => input)
            pattern = BoundTruePattern(location, source)
        end

    elseif @capture(source, ::T_)
        # bind type at macro expansion time.  It will be verified at runtime.
        bound_type = nothing
        try
            bound_type = Core.eval(state.mod, Expr(:block, location, T))
        catch ex
            error("$(location.file):$(location.line): Could not bind `$T` as a type (due to `$ex`).")
        end

        if !(bound_type isa Type)
            error("$(location.file):$(location.line): Attempted to match non-type `$T` as a type.")
        end
        pattern = BoundTypeTestPattern(location, T, input, bound_type)

    elseif @capture(source, subpattern_::T_)
        (pattern1, assigned) = bind_pattern!(location, :(::($T)), input, state, assigned)
        (pattern2, assigned) = bind_pattern!(location, subpattern, input, state, assigned)
        pattern = BoundAndPattern(location, source, BoundPattern[pattern1, pattern2])

    elseif @capture(source, T_(subpatterns__)) && is_possible_type_name(T)
        # struct pattern.
        len = length(subpatterns)
        named_fields = [pat.args[1] for pat in subpatterns if (pat isa Expr) && pat.head == :kw]
        named_count = length(named_fields)
        if named_count != length(unique(named_fields))
            error("$(location.file):$(location.line): Pattern `$source` has duplicate named arguments $named_fields.")
        elseif named_count != 0 && named_count != len
            error("$(location.file):$(location.line): Pattern `$source` mixes named and positional arguments.")
        end

        # bind type at macro expansion time
        (pattern0, assigned) = bind_pattern!(location, :(::($T)), input, state, assigned)
        bound_type = (pattern0::BoundTypeTestPattern).type
        patterns = BoundPattern[pattern0]

        # TODO: Instead of calling fieldnames, we should call a method that can be
        # overridden, e.g. by the implementation of `@auto_hash_equals_cached`, so
        # that macros can add fields that are not visible to pattern-matching.
        field_names = fieldnames(bound_type)

        match_positionally = named_count == 0
        if match_positionally
            fieldcount = length(field_names)
            if fieldcount != len
                error("$(location.file):$(location.line): Pattern field count is $len expected $fieldcount.")
            end
        end

        for i in 1:len
            pat = subpatterns[i]
            if match_positionally
                field_name = field_names[i]
                pattern_source = pat
            else
                @assert pat.head == :kw
                field_name = pat.args[1]
                pattern_source = pat.args[2]
            end

            # TODO: track the field type if it was declared
            fetch = BoundFetchFieldPattern(location, pattern_source, input, field_name)
            push!(patterns, fetch)
            field_temp = get_temp(state, fetch)
            (bound_subpattern, assigned) = bind_pattern!(
                location, pattern_source, field_temp, state, assigned)
            push!(patterns, bound_subpattern)
        end

        pattern = BoundAndPattern(location, source, patterns)

    elseif @capture(source, subpattern1_ && subpattern2_) ||
          (@capture(source, f_(subpattern1_, subpattern2_)) && f == :&)
        # conjunction: either `(a && b)` or `(a & b)` where `a` and `b` are patterns.
        (bp1, assigned) = bind_pattern!(location, subpattern1, input, state, assigned)
        (bp2, assigned) = bind_pattern!(location, subpattern2, input, state, assigned)
        pattern = BoundAndPattern(location, source, BoundPattern[bp1, bp2])

    elseif @capture(source, subpattern1_ || subpattern2_) ||
          (@capture(source, f_(subpattern1_, subpattern2_)) && f == :|)
        # disjunction: either `(a || b)` or `(a | b)` where `a` and `b` are patterns.
        (bp1, assigned1) = bind_pattern!(location, subpattern1, input, state, assigned)
        (bp2, assigned2) = bind_pattern!(location, subpattern2, input, state, assigned)

        # compute the common assignments.
        both = intersect(keys(assigned1), keys(assigned2))
        assigned = ImmutableDict{Symbol,Symbol}()
        for key in both
            v1 = assigned1[key]
            v2 = assigned2[key]
            if v1 == v2
                assigned = ImmutableDict(assigned, key => v1)
            else
                temp = get_temp(key)
                if v1 != temp
                    save = BoundFetchBindingPattern(location, source, v1, key)
                    bp1 = BoundAndPattern(location, source, [bp1, save])
                end
                if v2 != temp
                    save = BoundFetchBindingPattern(location, source, v2, key)
                    bp2 = BoundAndPattern(location, source, [bp2, save])
                end
                assigned = ImmutableDict(assigned, key => temp)
            end
        end
        pattern = BoundOrPattern(location, source, BoundPattern[bp1, bp2])

    elseif @capture(source, [subpatterns__]) || @capture(source, (subpatterns__,))
        # array or tuple
        splat_count = count(s -> s isa Expr && s.head == :..., subpatterns)
        if splat_count > 1
            error("$(location.file):$(location.line): More than one `...` in pattern `$source`.")
        end

        # produce a check that the input is an array (or tuple)
        patterns = BoundPattern[]
        base = (source.head == :vect) ? AbstractArray : Tuple
        pattern0 = BoundTypeTestPattern(location, base, input, base)
        push!(patterns, pattern0)
        len = length(subpatterns)

        ### TODO: make this more dry. Currently we repeat
        ###    fetch_foo = Fetch...(...)
        ###    foo_temp = get_temp(state, fetch_foo)
        ###    push!(patterns, fetch_foo)

        # produce a check that the length of the input is sufficient
        fetch_length = BoundFetchLengthPattern(location, source, input)
        length_temp = get_temp(state, fetch_length)
        push!(patterns, fetch_length)
        check_length =
            if splat_count != 0
                BoundRelationalTestPattern(
                    location, source, length_temp, :>=, length(subpatterns)-1)
            else
                BoundEqualValueTestPattern(
                    location, source, length_temp, length(subpatterns), ImmutableDict{Symbol, Symbol}())
            end
        push!(patterns, check_length)

        seen_splat = false
        for (i, subpattern) in enumerate(subpatterns)
            if subpattern isa Expr && subpattern.head == :...
                @assert length(subpattern.args) == 1
                @assert !seen_splat
                seen_splat = true
                fetch_range = BoundFetchRangePattern(
                    location, subpattern, input, i, len-i)
                push!(patterns, fetch_range)
                range_temp = get_temp(state, fetch_range)
                (patterni, assigned) = bind_pattern!(
                    location, subpattern.args[1], range_temp, state, assigned)
                push!(patterns, patterni)
            else
                index = seen_splat ? (i - len - 1) : i
                fetch_index = BoundFetchIndexPattern(location, subpattern, input, index)
                push!(patterns, fetch_index)
                index_temp = get_temp(state, fetch_index)
                (patterni, assigned) = bind_pattern!(
                    location, subpattern, index_temp, state, assigned)
                push!(patterns, patterni)
            end
        end
        pattern = BoundAndPattern(location, source, patterns)

    elseif @capture(source, subpattern_ where guard_)
        # guard
        (pattern0, assigned) = bind_pattern!(location, subpattern, input, state, assigned)
        pattern1 = BoundWhereTestPattern(location, guard, assigned)
        pattern = BoundAndPattern(location, source, [pattern0, pattern1])

    else
        error("$(location.file):$(location.line): Unregognized pattern syntax `$source`.")
    end

    return (pattern, assigned)
end
