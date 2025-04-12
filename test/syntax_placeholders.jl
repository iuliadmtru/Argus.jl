import Argus: has_binding, set_binding!, placeholder_unbind!, placeholder_fill!, _isequal

@testset "Metavariable" begin
    metavar = Metavariable(:y)
    # New metavariable, unbound.
    @test !has_binding(metavar)
    # Bound metavariable.
    syntax_data = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "2").data
    set_binding!(metavar, syntax_data)
    @test has_binding(metavar)
    @test metavar.binding.val === 2
    # Copy metavariable.
    metavar_copy = copy(metavar)
    @test isequal(metavar, metavar_copy)
    # Unbind metavariable.
    placeholder_unbind!(metavar)
    @test !has_binding(metavar)
    # Copy still bound.
    @test !isequal(metavar, metavar_copy)
    # Rebind metavariable.
    syntax_node = JuliaSyntax.parsestmt(JuliaSyntax.SyntaxNode, "x")
    @test placeholder_fill!(metavar, syntax_node)
    @test has_binding(metavar)
    @test metavar.binding.val === :x
    # Copy bound to other value.
    @test !isequal(metavar, metavar_copy)
    # New metavariable with different name but same binding.
    metavar_other_name = Metavariable(:z, syntax_node.data)
    @test !isequal(metavar, metavar_other_name)
    @test _isequal(metavar.binding, metavar_other_name.binding)
end
