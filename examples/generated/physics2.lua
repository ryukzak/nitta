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

    local result = a * constant1 + b * constant2 - c * constant3 + constant4
    send(result)

    local receivedValue = receive()

    local updatedA = a + receivedValue * constant1
    local updatedB = b - receivedValue * constant2
    local updatedC = c * receivedValue - constant3 + constant4

    physics(updatedA, updatedB, updatedC)
end

physics(0, 0, 0)
