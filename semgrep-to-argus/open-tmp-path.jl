# Match.
open("/tmp/blah1.txt", "r")

# Match.
open("/tmp/blah2.txt"; "r")

# Match.
open("/tmp/blah3.txt") do 
    print(1)
end

# TODO: Constant propagation.
x = "/tmp/blah4.txt"
open(x, "w")

# No match.
open("./tmp/blah5.txt")

# No match.
open("/home/user/tmp/blah.txt")
