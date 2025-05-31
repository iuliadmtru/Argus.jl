function _ParseError(code::String, first_byte::Int, last_byte::Int, msg::String)
    source = JuliaSyntax.SourceFile(code)
    diagnostic = JuliaSyntax.Diagnostic(first_byte, last_byte; error=msg)
    return JuliaSyntax.ParseError(source, [diagnostic], :none)
end
