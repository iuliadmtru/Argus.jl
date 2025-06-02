struct SyntaxError <: Exception
    msg::String
    file::Symbol
    line::Int
end

function Base.showerror(io::IO, err::SyntaxError)
    println(io, "ArgusSyntaxError:")
    println(io, err.msg)
    println(io, "@ $(err.file):$(err.line)")
end

struct BindingSetKeyError <: Exception
    key
end

function Base.showerror(io::IO, err::BindingSetKeyError)
    print(io, "BindingSetKeyError: ")
    println(io, "binding ", err.key, " not found")
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
