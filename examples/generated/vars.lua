function func(a, b, c, d)
    local sum1 = a + b
    local sum2 = c + d
    local product = sum1 * sum2
    local result = product + 100

    local square1 = sum1 * sum1
    local square2 = sum2 * sum2
    local sumSquares = square1 + square2
    local avg = (sumSquares + sumSquares) - sumSquares

    send(result)

    func(avg, square1, sumSquares, result)
end

func(1, 2, 3, 4)
