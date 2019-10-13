function pid(prev_err)
    local temperature_desired = 50
    local getValueSPI = receive()

    err = temperature_desired - getValueSPI

    local PID = err - prev_err
    send(PID)

    pid(err)
end

pid(0)
