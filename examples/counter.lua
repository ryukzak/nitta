function counter(x)
    local y = x + 1
    debug.trace(y)
    send(y)
    debug.trace(y)
    counter(y)
end
counter(0)
