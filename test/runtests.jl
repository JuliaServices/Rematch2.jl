module Rematch2Tests

import Rematch2: @match, MatchFailure, Rematch2, FirstSupertypeTester, next_supertype
using ReTest
import Random
import MacroTools

# include("test_FirstSupertypeTester.jl")

# This is very helpful for debugging the behavior of macros
macro print_expand(x)
    quote
        let
            a = @macroexpand $(esc(x))
            # println("\nexpanded:")
            # println(a)
            b = MacroTools.prewalk(MacroTools.rmlines, a)
            # println("\nrmlines:")
            # println(b)
            c = MacroTools.prewalk(MacroTools.unblock, b)
            # println("\nunblock:")
            # println(c)
            d = MacroTools.alias_gensyms(c)
            # println("alias_gensyms:")
            println(d)
        end
    end
end

include("rematch.jl")

retest(Rematch2Tests)

end # module
