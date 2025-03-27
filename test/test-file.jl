function f(a, b)
    y = a + b
    return y
end

a + b
a + b + c # (call-i a + b c); 4 children, all leaves; Semgrep finds this
c + a + b # (call-i c + a b); 4 children, all leaves; Semgrep doesn't find this

f(x) = "a"
f(x) = 2 + x
g(x) = 2 + x
