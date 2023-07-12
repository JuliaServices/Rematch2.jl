module Rematch2Tests

using Rematch2
using Rematch2: topological_sort
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
include("test_ismatch.jl")

retest(Rematch2Tests)

end # module
