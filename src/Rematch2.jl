module Rematch2

export @match, MatchFailure, @match_return, @match_fail, @ismatch

using MacroTools: MacroTools, @capture
using Base.Iterators: reverse
using Base: ImmutableDict
using OrderedCollections: OrderedDict

struct MatchFailure <: Exception
    value
end

# const fields only suppored >= Julia 1.8
macro _const(x)
    if VERSION >= v"1.8"
        Expr(:const, esc(x))
    else
        esc(x)
    end
end

is_expr(@nospecialize(e), head::Symbol) = e isa Expr && e.head == head
is_expr(@nospecialize(e), head::Symbol, n::Int) = is_expr(e, head) && length(e.args) == n
is_case(@nospecialize(e)) = is_expr(e, :call, 3) && e.args[1] == :(=>)

include("topological.jl")
include("immutable_vector.jl")
include("bound_pattern.jl")
include("binding.jl")
include("lowering.jl")
include("match_cases_simple.jl")
include("macros.jl")
include("automaton.jl")
include("pretty.jl")
include("match_cases_opt.jl")
include("match_return.jl")

end # module
