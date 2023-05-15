module Rematch2

export @match, @match2, MatchFailure, @xcase, @xcase_yield, @xcase_fail

using MacroTools: MacroTools, @capture
using Base: ImmutableDict

struct MatchFailure <: Exception
    value
end

include("TopologicalSort.jl")
include("ImmutableVector.jl")
include("BoundPattern.jl")
include("BindPattern.jl")
include("LowerPattern.jl")
include("MatchCases.jl")
include("Match.jl")
include("Match2Cases.jl")
include("Xcase.jl")

end # module
