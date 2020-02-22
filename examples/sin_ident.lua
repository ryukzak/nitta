function sin(x0, x1, x2, x3)
    
    local T = 0.001
    local OmegaN = 125.66  -- 2 * 3.1415 * 20
    local Pi2 = 6.28318    -- 3.14159 * 2
    local Alpha = 0.7
    local Beta = 5
    local Gamma = 80
    local RPow2 = 0
    local OmegaNDIVPi2 = 19.9994270417  -- (2 * 3.1415 * 20) / (3.14159 * 2)

    u = receive()

    local omega = OmegaN + Pi2 * x2

    local a = reg(u - x1 - x3)
    local b = Alpha * a * omega
    local c = x0 * omega
    local d = x1 * ( x0*x0 + x1*x1 - RPow2 )

    local dotx0 = x1 * omega
    local dotx1 = b - c - d
    local dotx2 = (- Beta) * a * x0 * omega
    local dotx3 = Gamma * a

    x0 = x0 + dotx0 * T
    x1 = x1 + dotx1 * T
    x2 = x2 + dotx2 * T
    x3 = x3 + dotx3 * T

    local freq = OmegaNDIVPi2 * x2
    send(freq)

    sin(x0, x1, x2, x3)
end
sin(0, 0, 0, 0)
