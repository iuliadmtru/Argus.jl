# Match.
if true
    println(1)
else
    println(2)
end

if cond1
    if cond2
        bla()
    elseif true || cond3  # Match.
        blu()
    else
        bli()
    end
else
    println(2)
end

x = false 

# Match.
if x && y || true
    println(1)
else
    println(2)
end

# Match.
if false && x
    println(1)
else
    println(2)
end

# Match.
if x || y && true
    println(1)
else
    println(2)
end

if x
    bla()
elseif true  # Match.
    blu()
end

# No match.
while true
    break
end

# No match.
x = foo() || false

# Match.
while x && true
    break
end
