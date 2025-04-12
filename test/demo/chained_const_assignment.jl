# Should match.
const a = b = 1

# Should match.
const a = b = c

# Should match.
const a = b = c = 1

# Should match.
const a = b = "abc"

# Should not match.
a = b = 1
