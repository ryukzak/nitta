function pid(prev_err)
    local Ki = 0
    local Kd = 0
    err = reg(receive())


    I = Ki * err
    D = Kd * (err - prev_err)

    local PID = err + I + D
    send(PID)

    pid(err)
end

pid(0)
