function counter(x)
    local y = x + 1
    send(y)
    counter(y)
end
counter(0)
