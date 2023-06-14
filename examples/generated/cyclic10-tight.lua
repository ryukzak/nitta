function cyclicProcess(a, b, c, d, e, f, g, h, i, j)
    local k = a + b + c + d + e + f + g + h + i + j
    send(k)

    local l = receive()
    local m = l * k + f * g - h * i
    send(m)

    local n = receive()
    local o = n + m + i * j - a * b
    send(o)

    local p = receive()
    local q = p - o * b + c * d - e * f
    send(q)

    local r = receive()
    local s = r + q * d - e * f + g * h - i * j
    send(s)

    cyclicProcess(m, n, o, p, q, r, s, m, n, o)
end

cyclicProcess(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
