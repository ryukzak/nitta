function fib(a, b)
    b, a = a + b, b
    send(a)
    send(b)
    fib(a, b)
end
fib(0, 1)