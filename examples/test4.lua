function fib(a, b)
    a, b = b, buffer(buffer(a) + buffer(b))
    fib(a, b)
end
fib(0, 1)
