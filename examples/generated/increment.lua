function increment(x)
    local step = receive()
    local y = x + step * 100
    send(y)
    increment(y)
end

increment(0)