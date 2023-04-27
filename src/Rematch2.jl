module Rematch2

export @match, MatchFailure

using MacroTools: MacroTools, @capture
using Base: ImmutableDict

struct MatchFailure <: Exception
    value
end

include("FirstSupertypeTester.jl")
include("BoundPattern.jl")
include("BindPattern.jl")
include("LowerPattern.jl")
include("MatchCases.jl")
include("Match.jl")

end # module
