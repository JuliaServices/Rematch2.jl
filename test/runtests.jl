module Rematch2Tests

using Rematch2
using ReTest
using Random

include("test_FirstSupertypeTester.jl")
include("rematch.jl")
include("rematch2.jl")

retest(Rematch2Tests)

end # module
