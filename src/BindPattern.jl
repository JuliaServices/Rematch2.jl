using Base: is_expr

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
    # computation producing the value (or the pattern variable that needs a temp)
    # to the symbol for the temp holding that value.
    assignments::Dict{Union{BoundFetchPattern, Symbol}, Symbol}

    # The set of type syntax forms that have asserted bindings in assertions
    asserted_types::Vector{Any}

    # Assertions that should be executed at runtime before the matching code.
    assertions::Vector{Any}

    # A dictionary used to intern CodePoint values in Match2Cases.
    intern::Dict

    function BinderState(mod::Module, input_variable::Symbol)
        new(
            mod,
            input_variable,
            Dict{BoundFetchPattern, Symbol}(),
            Vector{Pair{LineNumberNode, String}}(),
            Vector{Any}(),
            Dict()
        )
    end
end

function get_temp(state::BinderState, p::BoundFetchPattern)
    get!(gensym, state.assignments, p)
end
function get_temp(state::BinderState, p::BoundFetchBindingPattern)
    get!(() -> get_temp(state, p.variable), state.assignments, p)
end
function get_temp(state::BinderState, p::Symbol)
    get!(() -> gensym(string("saved_", p)), state.assignments, p)
end

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
        is_possible_type_name(t.args[2].value) ||
    t.head == :curly &&
        all(is_possible_type_name, t.args)
end

function bind_pattern!(
    location::LineNumberNode,
    source::Any,
    input::Symbol,
    state::BinderState,
    assigned::ImmutableDict{Symbol, Symbol})

    if source == :_
        # wildcard pattern
        pattern = BoundTruePattern(location, source)

    elseif !(source isa Expr || source isa Symbol)
        # a constant
        pattern = BoundEqualValueTestPattern(
            location, source, input, source, ImmutableDict{Symbol, Symbol}())

    elseif is_expr(source, [:macrocall, :quote]) ||
        (is_expr(source, :call) && source.args[1] == :Symbol) # the type `Symbol`
        # Objects of Julia expression tree types.  We treat them as constants, but
        # we should really match on their structure.  We might treat regular expressions
        # specially, handle interpolation inside, etc.
        # See #6, #30, #31, #32
        pattern = BoundEqualValueTestPattern(
            location, source, input, source, ImmutableDict{Symbol, Symbol}())

    elseif is_expr(source, :$)
        # an interpolation
        interpolation = source.args[1]
        interpolation0, assigned0 = subst_patvars(interpolation, assigned)
        pattern = BoundEqualValueTestPattern(
            location, interpolation, input, interpolation0, assigned0)

    elseif source isa Symbol
        # variable pattern (just a symbol)
        varsymbol::Symbol = source
        if haskey(assigned, varsymbol)
            # previously introduced variable.  Get the symbol holding its value
            var_value = assigned[varsymbol]
            pattern = BoundEqualValueTestPattern(
                location, source, input, var_value,
                ImmutableDict{Symbol, Symbol}(varsymbol, var_value))
        else
            # this patterns assigns the variable.
            assigned = ImmutableDict{Symbol, Symbol}(assigned, varsymbol, input)
            pattern = BoundTruePattern(location, source)
        end

    elseif is_expr(source, :(::), 1)
        # ::type
        T = source.args[1]
        T, where_clause = split_where(T, location)
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
        # Support `::T where ...` even though the where clause parses as
        # part of the type.
        pattern = join_where_clause(pattern, where_clause, location, assigned)

    elseif is_expr(source, :(::), 2)
        subpattern = source.args[1]
        T = source.args[2]
        T, where_clause = split_where(T, location)
        pattern1, assigned = bind_pattern!(location, :(::($T)), input, state, assigned)
        pattern2, assigned = bind_pattern!(location, subpattern, input, state, assigned)
        pattern = BoundAndPattern(location, source, BoundPattern[pattern1, pattern2])
        # Support `::T where ...` even though the where clause parses as
        # part of the type.
        pattern = join_where_clause(pattern, where_clause, location, assigned)

    elseif is_expr(source, :call) && is_possible_type_name(source.args[1])
        # struct pattern.
        # TypeName(patterns...)
        T = source.args[1]
        subpatterns = source.args[2:length(source.args)]
        len = length(subpatterns)
        named_fields = [pat.args[1] for pat in subpatterns if (pat isa Expr) && pat.head == :kw]
        named_count = length(named_fields)
        if named_count != length(unique(named_fields))
            error("$(location.file):$(location.line): Pattern `$source` has duplicate named arguments $named_fields.")
        elseif named_count != 0 && named_count != len
            error("$(location.file):$(location.line): Pattern `$source` mixes named and positional arguments.")
        end

        match_positionally = named_count == 0

        # bind type at macro expansion time
        pattern0, assigned = bind_pattern!(location, :(::($T)), input, state, assigned)
        bound_type = (pattern0::BoundTypeTestPattern).type
        patterns = BoundPattern[pattern0]
        field_names::Tuple = infer_fieldnames(bound_type, len, match_positionally, location)

        for i in 1:len
            pat = subpatterns[i]
            if match_positionally
                field_name = field_names[i]
                pattern_source = pat
            else
                @assert pat.head == :kw
                field_name = pat.args[1]
                pattern_source = pat.args[2]
                if !(field_name in field_names)
                    error("$(location.file):$(location.line): Type `$bound_type` has no field `$field_name`.")
                end
            end

            # TODO: track the field type if it was declared
            fetch = BoundFetchFieldPattern(location, pattern_source, input, field_name)
            push!(patterns, fetch)
            field_temp = get_temp(state, fetch)
            bound_subpattern, assigned = bind_pattern!(
                location, pattern_source, field_temp, state, assigned)
            push!(patterns, bound_subpattern)
        end

        pattern = BoundAndPattern(location, source, patterns)

    elseif is_expr(source, :(&&), 2)
        # conjunction: `(a && b)` where `a` and `b` are patterns.
        subpattern1 = source.args[1]
        subpattern2 = source.args[2]
        bp1, assigned = bind_pattern!(location, subpattern1, input, state, assigned)
        bp2, assigned = bind_pattern!(location, subpattern2, input, state, assigned)
        pattern = BoundAndPattern(location, source, BoundPattern[bp1, bp2])

    elseif is_expr(source, :call, 3) && source.args[1] == :&
        # conjunction: `(a & b)` where `a` and `b` are patterns.
        return bind_pattern!(location, Expr(:(&&), source.args[2], source.args[3]), input, state, assigned)

    elseif is_expr(source, :(||), 2)
        # disjunction: `(a || b)` where `a` and `b` are patterns.
        subpattern1 = source.args[1]
        subpattern2 = source.args[2]
        bp1, assigned1 = bind_pattern!(location, subpattern1, input, state, assigned)
        bp2, assigned2 = bind_pattern!(location, subpattern2, input, state, assigned)

        # compute the common assignments.
        both = intersect(keys(assigned1), keys(assigned2))
        assigned = ImmutableDict{Symbol, Symbol}()
        for key in both
            v1 = assigned1[key]
            v2 = assigned2[key]
            if v1 == v2
                assigned = ImmutableDict{Symbol, Symbol}(assigned, key, v1)
            else
                temp = get_temp(state, key)
                if v1 != temp
                    save = BoundFetchBindingPattern(location, source, v1, key)
                    bp1 = BoundAndPattern(location, source, BoundPattern[bp1, save])
                end
                if v2 != temp
                    save = BoundFetchBindingPattern(location, source, v2, key)
                    bp2 = BoundAndPattern(location, source, BoundPattern[bp2, save])
                end
                assigned = ImmutableDict{Symbol, Symbol}(assigned, key, temp)
            end
        end
        pattern = BoundOrPattern(location, source, BoundPattern[bp1, bp2])

    elseif is_expr(source, :call, 3) && source.args[1] == :|
        # disjunction: `(a | b)` where `a` and `b` are patterns.
        return bind_pattern!(location, Expr(:(||), source.args[2], source.args[3]), input, state, assigned)

    elseif is_expr(source, :tuple) || is_expr(source, :vect)
        # array or tuple
        subpatterns = source.args
        splat_count = count(s -> is_expr(s, :...), subpatterns)
        if splat_count > 1
            error("$(location.file):$(location.line): More than one `...` in pattern `$source`.")
        end

        # produce a check that the input is an array (or tuple)
        patterns = BoundPattern[]
        base = source.head == :vect ? AbstractArray : Tuple
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
                    location, source, length_temp, length(subpatterns),
                    ImmutableDict{Symbol, Symbol}())
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
                patterni, assigned = bind_pattern!(
                    location, subpattern.args[1], range_temp, state, assigned)
                push!(patterns, patterni)
            else
                index = seen_splat ? (i - len - 1) : i
                fetch_index = BoundFetchIndexPattern(location, subpattern, input, index)
                push!(patterns, fetch_index)
                index_temp = get_temp(state, fetch_index)
                patterni, assigned = bind_pattern!(
                    location, subpattern, index_temp, state, assigned)
                push!(patterns, patterni)
            end
        end
        pattern = BoundAndPattern(location, source, patterns)

    elseif is_expr(source, :where, 2)
        # subpattern where guard
        subpattern = source.args[1]
        guard = source.args[2]
        pattern0, assigned = bind_pattern!(location, subpattern, input, state, assigned)
        guard0, assigned0 = subst_patvars(guard, assigned)
        pattern1 = BoundWhereTestPattern(location, guard, guard0, assigned0)
        pattern = BoundAndPattern(location, source, BoundPattern[pattern0, pattern1])

    else
        error("$(location.file):$(location.line): Unregognized pattern syntax `$source`.")
    end

    return (pattern, assigned)
end

function push_pattern!(patterns::Vector{BoundPattern}, state::BinderState, pat::BoundFetchPattern)
    temp = get_temp(state, pat)
    push!(patterns, pat)
    temp
end

function split_where(T, location)
    type = T
    where_clause = nothing
    while type isa Expr && type.head == :where
        where_clause = (where_clause === nothing) ? type.args[2] : :($(type.args[2]) && $where_clause)
        type = type.args[1]
    end

    if !is_possible_type_name(type)
        error("$(location.file):$(location.line): Invalid type name: `$type`.")
    end

    return (type, where_clause)
end

function join_where_clause(pattern, where_clause, location, assigned)
    if where_clause === nothing
        return pattern
    else
        guard0, assigned0 = subst_patvars(where_clause, assigned)
        pattern1 = BoundWhereTestPattern(location, where_clause, guard0, assigned0)
        return BoundAndPattern(location, where_clause, BoundPattern[pattern, pattern1])
    end
end

#
# Infer which fields to match in a positional struct pattern by inspecting the set
# of constructors.  It would be nice to exclude constructors that have
# required keyword parameters, but the Julia APIs offer no simple way to determine
# which keyword parameters have defaults.  That's because keyword parameters without
# defaults are just rewritten into keyword parameters with defaults that throw an
# exception at runtime.  So we exclude functions that have any keyword parameters.
# If that ends up being problematic, we'll revisit the strategy.
#
function infer_fieldnames(type::Type, len::Int, match_positionally::Bool, location::LineNumberNode)
    members = try
        fieldnames(type)
    catch ex
        error("$(location.file):$(location.line): Could not determine the field names of `$type`.")
    end

    # If we're matching by keyword, we permit the use of any declared fields.
    match_positionally || return members

    # Search for constructor methods that have the correct number of parameters,
    # no keyword parameters, and are not varargs.
    meths = Method[methods(type)...]
    meths = filter(m -> !m.isva && length(Base.kwarg_decl(m))==0, meths)
    # drop the implicit var"#self#" argument
    argnames = map(m -> dropfirst(Base.method_argnames(m)), meths)
    # narrow to arg lists of the correct length where all parameter names correspond to members
    argnames = unique(filter(l -> length(l) == len && all(n -> n in members, l), argnames))

    if length(argnames) == 1
        # found a uniquely satisfying order for member names
        return (argnames[1]...,)
    elseif len == length(members)
        # no unique constructor, but the correct number of fields exist; use them
        return members
    elseif len > length(members)
        error("$(location.file):$(location.line): The type `$type` has $(length(members)) fields but the pattern expects $len fields.")
    else
        error("$(location.file):$(location.line): Cannot infer which $len of the $(length(members)) fields to match from any positional constructor for `$type`.")
    end
end
dropfirst(a) = a[2:length(a)]

#
# Replace each pattern variable reference with the temporary variable holding the
# value that corresponds to that pattern variable.
#
function subst_patvars(expr, assigned::ImmutableDict{Symbol, Symbol})
    new_assigned = ImmutableDict{Symbol, Symbol}()
    # postwalk(f, x) = walk(x, x -> postwalk(f, x), f)
    new_expr = MacroTools.postwalk(expr) do patvar
        if patvar isa Symbol
            tmpvar = get(assigned, patvar, nothing)
            if tmpvar isa Symbol
                if !haskey(new_assigned, patvar)
                    new_assigned = ImmutableDict{Symbol, Symbol}(new_assigned, patvar, tmpvar)
                end
                return Expr(:block, tmpvar)
            end
        end
        patvar
    end
    (new_expr, new_assigned)
end
