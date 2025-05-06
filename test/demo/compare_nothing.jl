# Should not match.
x = nothing

# Should match.
if x == nothing
    println(1)
else
    println(2)
end

# Should match.
if x != nothing
    println(1)
else
    println(2)
end

# Should match.
if nothing == x
    println(1)
else
    println(2)
end

# Should match.
if nothing != x
    println(1)
else
    println(2)
end

y=2 

# Should match.
if y ==2 && x == nothing
    println(1)
else
    println(2)
end

# Should match.
if x == nothing || y == 2
    println(1)
else
    println(2)
end

# Should not match.
if nothing === x
    println(1)
else
    println(2)
end

# Should not match.
if nothing !== x
    println(1)
else
    println(2)
end

# Should not match.
if x === nothing
    println(1)
else
    println(2)
end

# Should not match.
if isnothing(X)
    println(1)
else
    println(2)
end
