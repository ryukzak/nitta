function pid(I, prev_err)
    local Kp = 2
    local Ki = 0
    -- for current stage, we cannot use two name for same constant
    -- local Kd = 0

    local temperature_desired = 50
    local getValueSPI = receive()

    err = temperature_desired - getValueSPI
    P = Kp * err
    I = I + Ki * err
    D = Ki * (err - prev_err) -- Kd * (err - prev_err)

    local PID = P + I + D
    send(PID)

    pid(I, err)
end

pid(0, 0)
