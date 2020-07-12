function counter(x)
    local y = reg(0.7)
    x = y + x
    counter(x)
end
counter(0)
