function counter(x)
    local y = reg(0.7)
    x = y + x
    debug.trace("%.7f", y)
    counter(x)
end
counter(0)
