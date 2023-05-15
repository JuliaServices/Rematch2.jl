module Rematch2Tests

using Rematch2: @match, @match2, MatchFailure, Rematch2, @xcase, @xcase_fail, @xcase_yield
using ReTest
using Random
using MacroTools: MacroTools

include("utils.jl")
include("rematch.jl")
include("rematch2.jl")
include("topological.jl")
include("xcase.jl")

retest(Rematch2Tests)

end # module
