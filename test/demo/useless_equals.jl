# Should match.
if x == x
    println(1)
end

# Should match.
if x != x
    println(1)
end

# Should match.
if x === x
    println(1)
end

# Should match.
if x !== x
    println(1)
end

# TODO: Should not match.
# @test x == x
