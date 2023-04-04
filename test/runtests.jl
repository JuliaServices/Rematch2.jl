module Rematch2Tests

using Rematch2: Rematch2, FirstSupertypeTester, next_supertype
using ReTest
using Random

include("test_FirstSupertypeTester.jl")

retest(Rematch2Tests)

end # module
