struct ArgusSyntaxError <: Exception
    msg::String
    file::Symbol
    line::Int
end

function Base.showerror(io::IO, err::ArgusSyntaxError)
    println(io, "ArgusSyntaxError:")
    println(err.msg)
    println("@ $(err.file):$(err.line)")
end
