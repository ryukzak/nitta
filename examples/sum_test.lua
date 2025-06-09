function sum_test(a, b)
    local x = a - b
    sum_test(x, b)
end
sum_test(1, 1.123)
