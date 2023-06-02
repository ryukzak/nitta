function cyclicProcess(a, b, c, d, e)
    local f = a + b
    send(f)

    local g = receive()
    local h = g * f + c
    send(h)

    local i = receive()
    local j = i + h + d
    send(j)

    local k = receive()
    local l = k - j * e
    send(l)

    cyclicProcess(h, i, j, k, l)
end

cyclicProcess(1, 2, 3, 4, 5)