function physics(a, b, c)
    local pi = 3.14159
    local e = 2.71828
    local gravity = 9.8
    local speedOfLight = 299792458

    local constant1 = pi * gravity * gravity
    local constant2 = e * speedOfLight
    local constant3 = constant1 + constant2

    local result = a * constant1 + b * constant2 - c * constant3
    send(result)

    local receivedValue = receive()

    local updatedA = a + receivedValue * constant1
    local updatedB = b - receivedValue * constant2
    local updatedC = c * receivedValue - constant3

    physics(updatedA, updatedB, updatedC)
end

physics(0, 0, 0)
