function cyclicProcess(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    local u = a + b
    send(u)

    local v = receive()
    local w = v * u + c
    send(w)

    local x = receive()
    local y = x + w + d
    send(y)

    local z = receive()
    local aa = z - y * e
    send(aa)

    local bb = receive()
    local cc = bb + aa * f
    send(cc)

    local dd = receive()
    local ee = dd - cc * g
    send(ee)

    local ff = receive()
    local gg = ff + ee * h
    send(gg)

    local hh = receive()
    local ii = hh - gg * i
    send(ii)

    local jj = receive()
    local kk = jj + ii * j
    send(kk)

    local ll = receive()
    local mm = ll - kk * k
    send(mm)

    cyclicProcess(aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, ll, mm, n, o, p, q, r, s, t)
end

cyclicProcess(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
