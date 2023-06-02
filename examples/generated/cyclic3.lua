function cyclicProcess(a, b, c)
    local d = a + b + c
    send(d)

    local e = receive()
    local f = e * d
    send(f)

    local g = receive()
    local h = g - f
    send(h)

    cyclicProcess(f, h, g)
end

cyclicProcess(1, 2, 3)