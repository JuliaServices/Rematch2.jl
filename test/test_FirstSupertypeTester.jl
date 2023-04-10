using Rematch2: FirstSupertypeTester, next_supertype

const ntypes = 5 # 100
const ntests = 10 # 5000
const nrepeat = 3
const seed = 12345

Random.seed!(seed)

const all_types = Vector{Type}(map(1:ntypes) do i
    sym = Symbol("C", i)
    def = :(struct $sym end)
    eval(def)
    eval(sym)::Type
end)
struct CN end

@testset "FirstSupertypeTester.jl" begin

    test_types::Array{Type} = map(1:ntests) do i
        r::Int = rand(0:5) == 0 ? 0 : rand(1:ntypes)
        r == 0 ? CN : all_types[r]
    end
    test_start_index::Array{Int} = rand(1:ntypes, ntests)
    test_expected::Array{Int} = map(1:ntests) do i
        Rematch2.brute_next_supertype(test_types[i], all_types, test_start_index[i])
    end

    @testset "try many sequential accesses to the cache" begin
        stt = FirstSupertypeTester(all_types)
        for k::Int in 1:nrepeat
            for i::Int in 1:ntests
                @test next_supertype(test_types[i], stt, test_start_index[i]) == test_expected[i]
            end
        end
    end

    # @testset "try many concurrent access to the cache" begin
    #     stt = FirstSupertypeTester(all_types)
    #     for k::Int in 1:nrepeat
    #         all_tasks = map(1:ntests) do i
    #             @task next_supertype(test_types[i], stt, test_start_index[i]) == test_expected[i]
    #         end
    #         for task in all_tasks
    #             schedule(task)
    #         end
    #         for i in 1:ntests
    #             task = all_tasks[i]
    #             @testset "iteration $k join task $i" begin
    #                 @test fetch(task)
    #             end
    #         end
    #     end
    # end
end
