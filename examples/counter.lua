function counter(x)
    local y = x + 1
    send(y)
    trace(y)
    counter(y)
end
counter(0)
