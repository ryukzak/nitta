function func(a, b, c, d)
    local const1 = 1
    local const2 = 3.1415 * 20
    local const3 = 6.28318
    local const5 = 0.001
    local const6 = -1
    local const7 = const6 * const2
    local const8 = const5 * const7

    local sum1 = a + b
    local sum2 = c + d
    local product = sum1 * sum2
    local result = product + 100

    local square1 = sum1 * sum1
    local square2 = sum2 * sum2
    local sumSquares = square1 + square2
    local avg = (sumSquares + sumSquares) - sumSquares

    local dotx0 = b * const7
    local dotx1 = product - dotx0 - const8
    local dotx2 = const6 * const2 * a * product
    local dotx3 = const5 * (product - sum1)

    local r_x0 = a + dotx0 * const5
    local r_x1 = b + dotx1 * const5
    local r_x2 = c + dotx2 * const5
    local r_x3 = d + dotx3 * const5

    send(result)

    func(avg, square1, sumSquares, result)
end

func(1, 2, 3, 4)
