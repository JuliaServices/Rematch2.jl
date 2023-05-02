module Rematch2Tests

import Rematch2: @match, @match2, MatchFailure, Rematch2, FirstSupertypeTester, next_supertype
using ReTest
import Random
import MacroTools

# include("test_FirstSupertypeTester.jl")

include("utils.jl")
include("rematch.jl")
include("rematch2.jl")

retest(Rematch2Tests)

end # module
