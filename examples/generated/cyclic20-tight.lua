function cyclicProcess(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    local u = a + b + c + d + e + f + g + h + i + j
    send(u)

    local v = receive()
    local w = v * u + f * g - h * i
    send(w)

    local x = receive()
    local y = x + w + i * j - k * l + m * n
    send(y)

    local z = receive()
    local aa = z - y * l + m * n + o * p - q * r
    send(aa)

    local bb = receive()
    local cc = bb + aa * p - q * r + s * t - u * v + w * x
    send(cc)

    cyclicProcess(w, x, y, z, aa, cc, bb, cc, bb, aa, bb, cc, aa, bb, cc, aa, bb, cc, aa, bb)
end

cyclicProcess(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
