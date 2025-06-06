# Match.
rand() < 0.5

function some_rand_function(x)
    # Match.
    if rand() < 0.5
        println("Random")
    end
end

# No match.
rand() < 0.7  # this is fine

x = rand()
# No match.
if x < 0.5
    do_something()
else
    do_something_else()
end

# No match.
y = ok()
if y < 0.5
    do_something()
else
    do_something_else()
end

# No match.
x < 0.5 && action()

# No match.
flag = rand(Bool)

# Match.
if some_flag && rand() < 0.5 || other_flag
    println("Random")
end
