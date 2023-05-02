module Rematch2

export @match, @match2, MatchFailure

using MacroTools: MacroTools, @capture
using Base: ImmutableDict

struct MatchFailure <: Exception
    value
end

include("TopologicalSort.jl")
include("ImmutableVector.jl")
include("FirstSupertypeTester.jl")
include("BoundPattern.jl")
include("BindPattern.jl")
include("LowerPattern.jl")
include("MatchCases.jl")
include("Match.jl")
include("Match2Cases.jl")

end # module
