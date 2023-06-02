function matrixMultiplication()
    local a11 = receive()
    local a12 = receive()
    local a13 = receive()
    local a14 = receive()

    local a21 = receive()
    local a22 = receive()
    local a23 = receive()
    local a24 = receive()

    local a31 = receive()
    local a32 = receive()
    local a33 = receive()
    local a34 = receive()

    local a41 = receive()
    local a42 = receive()
    local a43 = receive()
    local a44 = receive()

    local b11 = receive()
    local b12 = receive()
    local b13 = receive()
    local b14 = receive()

    local b21 = receive()
    local b22 = receive()
    local b23 = receive()
    local b24 = receive()

    local b31 = receive()
    local b32 = receive()
    local b33 = receive()
    local b34 = receive()

    local b41 = receive()
    local b42 = receive()
    local b43 = receive()
    local b44 = receive()

    local c11 = a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41
    local c12 = a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42
    local c13 = a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43
    local c14 = a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44

    local c21 = a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41
    local c22 = a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42
    local c23 = a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43
    local c24 = a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44

    local c31 = a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41
    local c32 = a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42
    local c33 = a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43
    local c34 = a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44

    local c41 = a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41
    local c42 = a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42
    local c43 = a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43
    local c44 = a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44

    send(c11)
    send(c12)
    send(c13)
    send(c14)

    send(c21)
    send(c22)
    send(c23)
    send(c24)

    send(c31)
    send(c32)
    send(c33)
    send(c34)

    send(c41)
    send(c42)
    send(c43)
    send(c44)

    matrixMultiplication()
end

matrixMultiplication()
