function sum(a, b, c)
    e = a * b + c
    l = e + a
    sum(l, l, l)
end
sum(1, 1, 1)
