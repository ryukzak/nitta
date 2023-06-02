function cyclicProcess(a, b, c, d)
    local e = a + b + c + d
    send(e)

    local f = receive()
    local g = f * e
    send(g)

    local h = receive()
    local i = h + g
    send(i)

    local j = receive()
    local k = j * i
    send(k)

    cyclicProcess(g, i, k, j)
end

cyclicProcess(1, 2, 3, 4)