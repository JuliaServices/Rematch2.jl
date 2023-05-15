@testset "xcase tests" begin

@testset "simple uses work correctly" begin
    @test (@match2 Foo(1, 2) begin
        @xcase Foo(x, 2) => begin
            x
            @xcase_fail
        end
        @xcase Foo(1, x) => begin
            @xcase_yield x
            12
        end
    end) == 2
end

file = Symbol(@__FILE__)

@testset "uses of early-exit macros outside @xcase produce errors 1" begin
    let line = 0
        try
            line = (@__LINE__) + 3
            @eval @match2 1 begin
                1 => begin
                    @xcase_yield 2
                    12
                end
            end
            @test false
        catch ex
            @test ex isa LoadError
            e = ex.error
            @test e isa ErrorException
            @test e.msg == "$file:$line: @xcase_yield may only be used within the value of an @xcase."
        end
    end
end

@testset "uses of early-exit macros outside @xcase produce errors 2" begin
    let line = 0
        try
            line = (@__LINE__) + 3
            @eval @match2 1 begin
                1 => begin
                    @xcase_fail
                    12
                end
            end
            @test false
        catch ex
            @test ex isa LoadError
            e = ex.error
            @test e isa ErrorException
            @test e.msg == "$file:$line: @xcase_fail may only be used within the value of an @xcase."
        end
    end
end

@testset "nested uses do not interfere with each other" begin
    @test (@match2 1 begin
        @xcase 1 => begin
            t = @match2 1 begin
                @xcase 1 => begin
                    # yield from inner only
                    @xcase_yield 1
                    error()
                end
            end
            # yield from outer only
            @xcase_yield t + 1
            error()
        end
    end) == 2
end

end
