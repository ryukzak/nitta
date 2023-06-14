function fib(a, b)
    b, a = a + b, b
    fib(a, b)
end
fib(0, 1)

-- Value:    1 1 2 3 5 8
-- Cycle 1:  a b
-- Cycle 2:    a b
-- Cycle 3:      a b
-- Cycle 4:        a b
-- Cycle 5:          a b
