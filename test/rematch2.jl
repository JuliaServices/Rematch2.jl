
# @testset "Nested patterns" begin
#     e = Foo(1, 2)
#     @match e begin
#         Foo(x, y) => begin
#             @test x == 1 && y == 2
#             # x and y are bound here.  What if we try to use them again?
#             @match e begin
#                 Foo(y, x) where begin
#                     @match e begin
#                         Foo(y, x) where (y == 1) => begin
#                             @test y == 1 && x == 2
#                         end
#                     end
#                     @match e begin
#                         Foo(x, y) where (x == 1) => begin
#                             @test x == 1 && y == 2
#                         end
#                     end
#                     y == 1
#                 end => begin
#                     @test y == 1 && x == 2
#                 end
#             end
#             @test x == 1 && y == 2
#         end
#     end
# end
