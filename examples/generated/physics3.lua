function physics(a, b, c)
    local pi = 3.14159
    local e = 2.71828
    local gravity = 9.8
    local speedOfLight = 299792458
    local avogadro = 6.022 * (10 * 10 * 10 * 10 * 10 * 10)
    local boltzmann = 1.380649 * (10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10 * 10)

    local constant1 = pi * gravity * gravity
    local constant2 = e * speedOfLight
    local constant3 = constant1 + constant2
    local constant4 = avogadro * boltzmann
    local constant5 = constant1 * constant2

    local var1 = constant1 * a + constant2 * b - constant3 * c
    local var2 = constant4 + constant5 * a - constant1 * b + constant2 * c
    local var3 = constant1 * a - constant2 * b + constant3 * c - constant4 * constant5
    local var4 = var1 + var2 * var3
    local var5 = var4 * constant1 + var2 - var3 + constant4
    local var6 = var5 * constant5

    local result = var6 * var1 - var2 + var3 + var4
    send(result)

    local receivedValue = receive()

    local updatedA = a + receivedValue * var1
    local updatedB = b - receivedValue * var2
    local updatedC = c * receivedValue - var3 + var4 + var5

    physics(updatedA, updatedB, updatedC)
end

physics(0, 0, 0)
