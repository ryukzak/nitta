function matrixMultiplication()
    local a11 = receive()
    local a12 = receive()
    local a13 = receive()
    local a21 = receive()
    local a22 = receive()
    local a23 = receive()
    local a31 = receive()
    local a32 = receive()
    local a33 = receive()

    local b11 = receive()
    local b12 = receive()
    local b13 = receive()
    local b21 = receive()
    local b22 = receive()
    local b23 = receive()
    local b31 = receive()
    local b32 = receive()
    local b33 = receive()

    local c11 = a11 * b11 + a12 * b21 + a13 * b31
    local c12 = a11 * b12 + a12 * b22 + a13 * b32
    local c13 = a11 * b13 + a12 * b23 + a13 * b33
    local c21 = a21 * b11 + a22 * b21 + a23 * b31
    local c22 = a21 * b12 + a22 * b22 + a23 * b32
    local c23 = a21 * b13 + a22 * b23 + a23 * b33
    local c31 = a31 * b11 + a32 * b21 + a33 * b31
    local c32 = a31 * b12 + a32 * b22 + a33 * b32
    local c33 = a31 * b13 + a32 * b23 + a33 * b33

    send(c11)
    send(c12)
    send(c13)
    send(c21)
    send(c22)
    send(c23)
    send(c31)
    send(c32)
    send(c33)

    matrixMultiplication()
end

matrixMultiplication()
