-- reference implementation in sin_ident/main.go
function sin(x0, x1, x2, x3)
    local Alpha = 0.699997
    local Beta = 5
    local Gamma = 80
    -- local R = 1
    local RPow2 = 1 -- 1 * 1
    local OmegaN = 2 * 3.1415 * 20
    local Pi2 = 6.28318    -- 3.14159 * 2
    local OmegaNDIVPi2 = 19.9994270417  -- (2 * 3.1415 * 20) / (3.14159 * 2)
    local T = 0.001

    u = receive()
    local tmp1 = u - x1 - x3
    local tmp2 = x0*x0 + x1*x1 - RPow2

    local omega = OmegaN + Pi2 * x2

    local a = Alpha * tmp1 * omega
    local b = x0 * omega
    local c = x1 * tmp2

    local dotx0 = x1 * omega
    local dotx1 = a - b - c
    local dotx2 = -1 * omega * x0 * Beta * tmp1
    local dotx3 = Gamma * tmp1

    local r_x0 = x0 + dotx0 * T
    local r_x1 = x1 + dotx1 * T
    local r_x2 = x2 + dotx2 * T
    local r_x3 = x3 + dotx3 * T

    local freq = OmegaNDIVPi2 + x2
    send(freq)

    sin(r_x0, r_x1, r_x2, r_x3)
end
sin(0, 0, 0, 0)
