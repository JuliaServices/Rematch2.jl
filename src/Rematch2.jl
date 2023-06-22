module Rematch2

export @match, @match2, MatchFailure, @match_return, @match_fail

using MacroTools: MacroTools
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
include("Return.jl")

end # module
