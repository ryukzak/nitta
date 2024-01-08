function cyclicProcess(a, b, c, d, e, f, g, h, i, j)
    local k = a + b
    send(k)

    local l = receive()
    local m = l * k + c
    send(m)

    local n = receive()
    local o = n + m + d
    send(o)

    local p = receive()
    local q = p - o * e
    send(q)

    local r = receive()
    local s = r * q + f + g - h * i + j
    send(s)

    cyclicProcess(m, n, o, p, q, s, m, n, o, p)
end

cyclicProcess(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
