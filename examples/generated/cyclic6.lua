function cyclicProcess(a, b, c, d, e, f)
    local g = a + b
    send(g)

    local h = receive()
    local i = h * g + c
    send(i)

    local j = receive()
    local k = j + i + d
    send(k)

    local l = receive()
    local m = l - k * e + f
    send(m)

    cyclicProcess(i, j, k, l, m, g)
end

cyclicProcess(1, 2, 3, 4, 5, 6)
