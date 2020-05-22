function pid(I, prev_err)
    local Kp = 2
    local Ki = 0
    local Kd = 0

    local temperature_desired = 50
    local getValueSPI = receive()
    trace(getValueSPI)

    err = temperature_desired - getValueSPI
    trace(err)
    P = Kp * err
    I = I + Ki * err
    D = Kd * (err - prev_err)
    trace(P, I, D)

    local PID = P + I + D
    send(PID)

    trace(PID)
    pid(I, err)
end

pid(0, 0)
