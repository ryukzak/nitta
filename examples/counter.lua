function counter(x)
    local y = reg(1.5)
    y = y + x
    counter(y)
end
counter(0)
