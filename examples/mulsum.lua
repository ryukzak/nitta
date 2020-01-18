function sum(a, b, c)
    c = a * b + c
    sum(c, c, c)
end
sum(1, 1, 1)
