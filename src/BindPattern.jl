
#
# Persistent data that we use across different patterns, to ensure the same computations
# are always represented by the same synthetic variables.  We use this during lowering
# and also during code generation, since it holds some of the state required during code
# generation (such as assertions and assignments)
#
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

    # A counter used to dispense unique integers to make prettier gensyms
    num_gensyms::Ref{Int}

    function BinderState(mod::Module)
        new(
            mod,
            gensym("input_value"),
            Dict{BoundFetchPattern, Symbol}(),
            Vector{Pair{LineNumberNode, String}}(),
            Vector{Symbol}(),
            Dict(),
            Ref{Int}(0)
        )
    end
end
function gensym(base::String, state::BinderState)::Symbol
    s = gensym("$(base)_$(state.num_gensyms[])")
    state.num_gensyms[] += 1
    s
end
gensym(base::String)::Symbol = Base.gensym(base)

#
# Pretty-printing utilities helpful for displaying the state machine
#
pretty(io::IO, p::BoundPattern, state::BinderState) = pretty(io, p)
function pretty(io::IO, p::BoundFetchPattern, state::BinderState)
    temp = get_temp(state, p)
    pretty(io, temp)
    print(io, " := ")
    pretty(io, p)
end
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
function pretty(io::IO, s::Symbol)
    print(io, pretty_name(s))
end
function pretty_name(s::Symbol)
    s = string(s)
    if startswith(s, "##")
        string("«", simple_name(s), "»")
    else
        s
    end
end
struct FrenchName; s::Symbol; end
Base.show(io::IO, x::FrenchName) = print(io, pretty_name(x.s))
function pretty(io::IO, expr::Expr)
    b = MacroTools.prewalk(MacroTools.rmlines, expr)
    c = MacroTools.prewalk(MacroTools.unblock, b)
    print(io, MacroTools.postwalk(c) do var
        (var isa Symbol) ? FrenchName(var) : var
    end)
end

#
# Get the "base" name of a symbol (remove synthetic ## additions)
#
function simple_name(s::Symbol)
    simple_name(string(s))
end
function simple_name(n::String)
    if startswith(n, "##")
        n1 = n[3:length(n)]
        last = findlast('#', n1)
        (last isa Int) ? n1[1:(last-1)] : n1
    else
        n
    end
end

#
# Generate a fresh synthetic variable whose name hints at its purpose.
#
function gentemp(p)::Symbol
    error("not implemented: gentemp(::$(typeof(p)))")
end
function gentemp(p::BoundFetchFieldPattern)::Symbol
    gensym(string(simple_name(p.input), ".", p.field_name))
end
function gentemp(p::BoundFetchIndexPattern)::Symbol
    gensym(string(simple_name(p.input), "[", p.index, "]"))
end
function gentemp(p::BoundFetchRangePattern)::Symbol
    gensym(string(simple_name(p.input), "[", p.first_index, ":(length-", p.from_end, ")]"))
end
function gentemp(p::BoundFetchLengthPattern)::Symbol
    gensym(string("length(", simple_name(p.input), ")"))
end

#
# The following are special bindings used to handle the point where
# a disjunction merges when and two sides have different bindings.
# In dataflow-analysis terms, this is represented by a phi function.
# This is a synthetic variable to hold the value that should be used
# to hold the value after the merge point.
#
function get_temp(state::BinderState, p::BoundFetchPattern)::Symbol
    get!(state.assignments, p) do; gentemp(p); end
end
function get_temp(state::BinderState, p::BoundFetchExpressionPattern)::Symbol
    get!(state.assignments, p) do
        if p.key isa Symbol
            p.key
        else
            gensym("where", state)
        end
    end
end

#
# We restrict the struct pattern to require something that looks like
# a type name before the open paren.  This improves the diagnostics
# for error cases like `(a + b)`, which produces an analogous Expr node
# but with `+` as the operator.
#
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

    elseif (!(source isa Expr || source isa Symbol) ||
        @capture(source, _quote_macrocall) ||
        @capture(source, Symbol(_)) # questionable
        )
        # a constant
        pattern = BoundEqualValueTestPattern(
            location, source, input, source, ImmutableDict{Symbol, Symbol}())

    elseif source isa Expr && source.head == :$
        # an interpolation
        interpolation = source.args[1]
        interpolation0, assigned0 = subst_patvars(interpolation, assigned)
        pattern = BoundEqualValueTestPattern(
            location, interpolation, input, interpolation0, assigned0)

    elseif @capture(source, varsymbol_Symbol)
        # variable pattern (just a symbol)
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

    elseif @capture(source, ::T_)
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
        pattern = join_where_clause(pattern, where_clause, location, state, assigned)

    elseif @capture(source, subpattern_::T_)
        T, where_clause = split_where(T, location)
        pattern1, assigned = bind_pattern!(location, :(::($T)), input, state, assigned)
        pattern2, assigned = bind_pattern!(location, subpattern, input, state, assigned)
        pattern = BoundAndPattern(location, source, BoundPattern[pattern1, pattern2])
        # Support `::T where ...` even though the where clause parses as
        # part of the type.
        pattern = join_where_clause(pattern, where_clause, location, state, assigned)

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

            field_temp = push_pattern!(patterns, state,
                BoundFetchFieldPattern(location, pattern_source, input, field_name))
            bound_subpattern, assigned = bind_pattern!(
                location, pattern_source, field_temp, state, assigned)
            push!(patterns, bound_subpattern)
        end

        pattern = BoundAndPattern(location, source, patterns)

    elseif @capture(source, subpattern1_ && subpattern2_) ||
          (@capture(source, f_(subpattern1_, subpattern2_)) && f == :&)
        # conjunction: either `(a && b)` or `(a & b)` where `a` and `b` are patterns.
        bp1, assigned = bind_pattern!(location, subpattern1, input, state, assigned)
        bp2, assigned = bind_pattern!(location, subpattern2, input, state, assigned)
        pattern = BoundAndPattern(location, source, BoundPattern[bp1, bp2])

    elseif @capture(source, subpattern1_ || subpattern2_) ||
          (@capture(source, f_(subpattern1_, subpattern2_)) && f == :|)
        # disjunction: either `(a || b)` or `(a | b)` where `a` and `b` are patterns.
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
                # Every phi gets its own distinct variable.  We do not share them
                # between patterns.
                temp = gensym(string("phi_", key), state)
                if v1 != temp
                    save = BoundFetchExpressionPattern(location, source, v1, ImmutableDict(key => v1), temp)
                    bp1 = BoundAndPattern(location, source, BoundPattern[bp1, save])
                end
                if v2 != temp
                    save = BoundFetchExpressionPattern(location, source, v2, ImmutableDict(key => v2), temp)
                    bp2 = BoundAndPattern(location, source, BoundPattern[bp2, save])
                end
                assigned = ImmutableDict{Symbol, Symbol}(assigned, key, temp)
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
        base = source.head == :vect ? AbstractArray : Tuple
        pattern0 = BoundTypeTestPattern(location, base, input, base)
        push!(patterns, pattern0)
        len = length(subpatterns)

        # produce a check that the length of the input is sufficient
        length_temp = push_pattern!(patterns, state,
            BoundFetchLengthPattern(location, source, input))
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
                range_temp = push_pattern!(patterns, state,
                    BoundFetchRangePattern(location, subpattern, input, i, len-i))
                patterni, assigned = bind_pattern!(
                    location, subpattern.args[1], range_temp, state, assigned)
                push!(patterns, patterni)
            else
                index = seen_splat ? (i - len - 1) : i
                index_temp = push_pattern!(patterns, state,
                    BoundFetchIndexPattern(location, subpattern, input, index))
                patterni, assigned = bind_pattern!(
                    location, subpattern, index_temp, state, assigned)
                push!(patterns, patterni)
            end
        end
        pattern = BoundAndPattern(location, source, patterns)

    elseif @capture(source, subpattern_ where guard_)
        # guard
        pattern0, assigned = bind_pattern!(location, subpattern, input, state, assigned)
        pattern1 = shred_where_clause(guard, false, location, state, assigned)
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

function join_where_clause(pattern, where_clause, location, state, assigned)
    if where_clause === nothing
        return pattern
    else
        pattern1 = shred_where_clause(where_clause, false, location, state, assigned)
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

# Shred a `where` clause into its component parts, conjunct by conjunct.  If necessary,
# we push negation operators down.  This permits us to share the parts of a where clause
# between different rules.
#
function shred_where_clause(
    guard::Any,
    inverted::Bool,
    location::LineNumberNode,
    state::BinderState,
    assigned::ImmutableDict{Symbol, Symbol})::BoundPattern
    if @capture(guard, !g_)
        return shred_where_clause(g, !inverted, location, state, assigned)
    elseif @capture(guard, g1_ && g2_) || @capture(guard, g1_ || g2_)
        left = shred_where_clause(g1, inverted, location, state, assigned)
        right = shred_where_clause(g2, inverted, location, state, assigned)
        # DeMorgan's law:
        #     `!(a && b)` => `!a || !b`
        #     `!(a || b)` => `!a && !b`
        result_type = (inverted == (guard.head == :&&)) ? BoundOrPattern : BoundAndPattern
        return result_type(location, guard, BoundPattern[left, right])
    else
        (guard0, assigned0) = subst_patvars(guard, assigned)
        fetch = BoundFetchExpressionPattern(location, guard, guard0, assigned0, nothing)
        temp = get_temp(state, fetch)
        test = BoundWhereTestPattern(location, guard, temp, inverted)
        return BoundAndPattern(location, guard, BoundPattern[fetch, test])
    end
end

#
# Replace each pattern variable reference with the temporary variable holding the
# value that corresponds to that pattern variable.
#
function subst_patvars(expr, assigned::ImmutableDict{Symbol, Symbol})
    new_assigned = ImmutableDict{Symbol, Symbol}()
    new_expr = MacroTools.postwalk(expr) do patvar
        if patvar isa Symbol
            tmpvar = get(assigned, patvar, nothing)
            if tmpvar isa Symbol
                if !haskey(new_assigned, patvar)
                    new_assigned = ImmutableDict{Symbol, Symbol}(new_assigned, patvar, tmpvar)
                end
                # Prevent the variable from being assigned to in user code
                return Expr(:block, tmpvar)
            end
        end
        patvar
    end
    (new_expr, new_assigned)
end
