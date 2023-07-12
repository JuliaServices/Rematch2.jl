module Rematch2

export @match, @match2, MatchFailure, @match_return, @match_fail, @ismatch

using MacroTools: MacroTools, @capture
using Base.Iterators: reverse
using Base: ImmutableDict

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

include("TopologicalSort.jl")
include("ImmutableVector.jl")
include("BoundPattern.jl")
include("BindPattern.jl")
include("LowerPattern.jl")
include("MatchCases.jl")
include("Match.jl")
include("StateMachine.jl")
include("Match2Cases.jl")
include("Return.jl")

end # module
