function mul(a, b)
    local x = a * b
    mul(x, b)
end
mul(1, 1.123)
