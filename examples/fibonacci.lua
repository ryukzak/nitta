function fib(a, b)
    -- b = a + b
    -- a = b
    -- b, a = a + b, b
    b, a = a, b
    fib(a, b)
end
fib(0, 1)
