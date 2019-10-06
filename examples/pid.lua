function pid(I, prev_err) 
    local Kp = 2
    local Ki = 0
    local Kd = 0

    local temperature_desired = 50
    local getValueSPI = receive()

    err = temperature_desired - getValueSPI

    P = Kp * err
    I = I + Ki * err
    D = Kd * reg(err - prev_err)

    local PID = P + I + D
    send(PID)

    pid(I, err)
end

pid(0, 0)
