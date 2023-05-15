"""
    @xcase pattern => value

This is an enhanced way to write cases for the @match macro.
It permits early-exit from the value of a @match case.
The programmer may write the value as a `begin ... end` and then,
within the value, the programmer may write

    @xcase_fail

to cause the case to terminate as if its pattern had failed.
This permits cases to perform some computation before deciding if the
rule *really* matched.

To terminate with suceess, just have the last expression yield the
value desired for the case as usual.  To terminate **early** with success,
write

    @xcase_yield value
"""
macro xcase end

# These are rewritten during expansion of the `@xcase` macro,
# so the actual macro should not be used.
macro xcase_fail()
    error("$(__source__.file):$(__source__.line): @xcase_fail may only be used within the value of an @xcase.")
end
# These are rewritten during expansion of the `@xcase` macro,
# so the actual macro should not be used.
macro xcase_yield(x)
    error("$(__source__.file):$(__source__.line): @xcase_yield may only be used within the value of an @xcase.")
end

#
# We implement xcase as follows:
#
# Given a case (part of a @match)
#
#    @xcase pattern => value
#
# We create two synthetic names: one for a `label`, and one for an intermediate `temp`.
# Then we rewrite `value` into `value'` by replacing every occurrence of
#
#    @xcase_yield value
#
# with
#
#    begin
#        $temp = $value
#        @goto $label
#    end
#
# and every occurrence of
#
#    @xcase_fail
#
# With
#
#    @xcase_yield $MatchFailure
#
# And then we replace the whole `@xcase pattern => value` with
#
#    pattern where begin
#        $temp = $value'
#        @label $label
#        $tmp !== $MatchFailure
#        end => $temp
#
# Note that we are using the type `MatchFailure` as a sentinel value to indicate that the
# match failed.  Therefore, don't use the @xcase macro for cases in which `MatchFailure`
# is a possible result.
#
macro xcase(case)
    if !(@capture(case, pattern_ => result_))
        error("$(location.file):$(location.line): Unrecognized @xcase syntax: `$case`.")
    end

    value = gensym("value")
    label = gensym("label")
    rewritten_result = MacroTools.prewalk(result) do p
        p isa Expr || return p
        if p.head == :macrocall && length(p.args) == 3 && p.args[1] == :var"@xcase_yield"
            # @xcase_yield -> :($value = $e; @goto $label)
            Expr(:block, p.args[2], :($value = $(p.args[3])), :(@goto $label))
        elseif p.head == :macrocall && length(p.args) == 2 && p.args[1] == :var"@xcase_fail"
            # @xcase_fail -> :($value = $MatchFaulure; @goto $label)
            Expr(:block, p.args[2], :($value = $MatchFailure), :(@goto $label))
        elseif p.head == :macrocall && length(p.args) == 3 && p.args[1] == :var"@xcase"
            # Nested uses of @xcase should be treated as independent
            macroexpand(__module__, p, recursive = false)
        else
            p
        end
    end

    where_expr = Expr(:block, :($value = $rewritten_result), :(@label $label), :($value !== $MatchFailure))
    new_pattern = :($pattern where $where_expr)
    new_result = value
    esc(:($new_pattern => $new_result))
end
