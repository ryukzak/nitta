function matrixMultiplication()
    local a1 = receive()
    local a2 = receive()
    local a3 = receive()

    local b1 = receive()
    local b2 = receive()
    local b3 = receive()

    local c = a1 * b1 + a2 * b2 + a3 * b3

    send(c)

    matrixMultiplication()
end

matrixMultiplication()
