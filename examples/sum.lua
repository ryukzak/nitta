function sum(a, b, c)
    local d = a + b + c
    trace(d)
    sum(d, d, d)
end
sum(0,0,0)
