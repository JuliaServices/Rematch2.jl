module Rematch2

export @match, @match2, MatchFailure

using MacroTools: MacroTools, @capture

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
include("StateMachine.jl")
include("Match2Cases.jl")

end # module
