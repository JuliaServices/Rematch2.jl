module Rematch2Tests

using Rematch2: Rematch2, FirstSupertypeTester, next_supertype
using ReTest
using Random

ntypes::Int = 100
ntests::Int = 5000
nrepeat::Int = 3
seed::Int = 12345
Random.seed!(seed)

all_types::Vector{Type} = map(1:ntypes) do i
    sym = Symbol("C", i)
    def = :(struct $sym end)
    eval(def)
    eval(sym)
end
struct CN end

@testset "Rematch2.jl" begin
    test_types::Array{Type} = map(1:ntests) do i
        r::Int = rand(0:5) == 0 ? 0 : rand(1:ntypes)
        r == 0 ? CN : all_types[r]
    end
    test_start_index::Array{Int} = rand(1:ntypes, ntests)
    test_expected::Array{Int} = map(1:ntests) do i
        Rematch2.brute_next_supertype(test_types[i], all_types; :start_index => test_start_index[i])
    end

    @testset "try many sequential accesses to the cache" begin
        stt = FirstSupertypeTester(all_types)
        for k::Int in 1:nrepeat
            for i::Int in 1:ntests
                @test next_supertype(test_types[i], stt; :start_index => test_start_index[i]) == test_expected[i]
            end
        end
    end

    @testset "try many concurrent access to the cache" begin
        stt = FirstSupertypeTester(all_types)
        for k::Int in 1:nrepeat
            all_tasks = map(1:ntests) do i
                @task next_supertype(test_types[i], stt; :start_index => test_start_index[i]) == test_expected[i]
            end
            for task in all_tasks
                schedule(task)
            end
            for i in 1:ntests
                task = all_tasks[i]
                @testset "iteration $k join task $i" begin
                    @test fetch(task)
                end
            end
        end
    end
end

retest(Rematch2Tests)

end # module
