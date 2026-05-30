@testset "CommentPattern" begin

    @testset "Invalid syntax" begin
        @test_throws SyntaxError @macroexpand @comment x
    end

    @testset "Comment pattern matching" begin
        let
            src = """
            bla
            # bla
            bli # bla
            # another bla
            """
            comment = @comment "bla"
            src = parseall(SyntaxNode, src; filename="bla.jl")
            matches = comment_match_all(comment, src).matches
            @test length(matches) == 2
            @test source_location(matches[1]) == (2, 1)
            @test source_location(matches[2]) == (3, 5)
            @test filename(matches[1]) == filename(matches[2]) == "bla.jl"
        end
        let
            src = """
            bla
            # bla
            # abla
            # ablab
            """
            comment = @comment r"a.*a"
            matches = comment_match_all(comment, parseall(SyntaxNode, src)).matches
            @test length(matches) == 1
            @test source_location(matches[1]) == (3, 1)
        end
        let
            src = """
            # bl🧐
            # bla
            # abla
            #= abla =#
            #=
            abla
            =#
            """
            comment = @comment r"a.*a"
            matches = comment_match_all(comment, parseall(SyntaxNode, src)).matches
            @test length(matches) == 2
            @test source_location(matches[1]) == (3, 1)
            @test source_location(matches[2]) == (4, 1)
        end
        let
            src = """
            bla
            # bla
            # abla
            #= abla =#
            #=
            abla
            =#
            # ablab
            #=
            bla
             babla
            =#
            """
            comment = @comment r"[\S\s]*a.*a[\S\s]*"
            matches = comment_match_all(comment, parseall(SyntaxNode, src)).matches
            @test length(matches) == 5
            @test source_location(matches[1]) == (3, 1)
            @test source_location(matches[2]) == (4, 1)
            @test source_location(matches[3]) == (5, 1)
            @test source_location(matches[4]) == (8, 1)
            @test source_location(matches[5]) == (9, 1)
        end
    end
    
end
