module Rematch2Tests

using Rematch2: @match, @match2, MatchFailure, Rematch2, @match_fail, @match_return
using ReTest
using Random
using MacroTools: MacroTools

include("utils.jl")
include("rematch.jl")
include("rematch2.jl")
include("coverage.jl")
include("nontrivial.jl")
include("topological.jl")
include("match_return.jl")

retest(Rematch2Tests)

end # module
