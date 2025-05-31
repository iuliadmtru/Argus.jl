struct ArgusSyntaxError <: Exception
    msg::String
    file::Symbol
    line::Int
end

function Base.showerror(io::IO, err::ArgusSyntaxError)
    println(io, "ArgusSyntaxError:")
    println(io, err.msg)
    println(io, "@ $(err.file):$(err.line)")
end
