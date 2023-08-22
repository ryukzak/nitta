function cyclicProcess(a, b)
    local c = a + b
    send(c)

    local d = receive()
    local e = d * c
    send(e)

    cyclicProcess(e, d)
end

cyclicProcess(1, 2)
