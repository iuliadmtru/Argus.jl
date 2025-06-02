struct SyntaxError <: Exception
    msg::String
    file::Union{Nothing, Symbol, String}
    line::Union{Nothing, Int}
end
SyntaxError(msg::String) = SyntaxError(msg, nothing, nothing)

function Base.showerror(io::IO, err::SyntaxError)
    print(io, "SyntaxError: ")
    println(io, err.msg)
    isnothing(err.file) && return
    println(io, "@ $(err.file):$(err.line)")
end

struct MatchError <: Exception
    eval_result
end

function Base.showerror(io::IO, err::MatchError)
    print(io, "MatchError: ")
    println(io,
            "Fail condition evaluated to ",
            typeof(err.eval_result),
            " instead of Bool (`",
            err.eval_result,
            "`)")
end
