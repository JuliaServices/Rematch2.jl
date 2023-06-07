module Rematch2Tests

using Rematch2: @match, @match2, MatchFailure, Rematch2
using ReTest
using Random
using MacroTools: MacroTools

include("utils.jl")
include("rematch.jl")
include("rematch2.jl")
include("coverage.jl")
include("topological.jl")

retest(Rematch2Tests)

end # module
