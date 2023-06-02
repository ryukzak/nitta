function matrixMultiplication()
    local a11 = receive()
    local a12 = receive()
    local a21 = receive()
    local a22 = receive()

    local b11 = receive()
    local b12 = receive()
    local b21 = receive()
    local b22 = receive()

    local c11 = a11 * b11 + a12 * b21
    local c12 = a11 * b12 + a12 * b22
    local c21 = a21 * b11 + a22 * b21
    local c22 = a21 * b12 + a22 * b22

    send(c11)
    send(c12)
    send(c21)
    send(c22)

    matrixMultiplication()
end

matrixMultiplication()
