function pid(prev_error) 
    local temperature_desired = 50
    local getValueSPI = receive()
    
    error = temperature_desired - getValueSPI
    D = error - reg(prev_error)
    send(D)

    pid(error)
end

pid(0)



-- function pid(I, prev_error) 
--     local Kp = 2
--     local Ki = 0
--     local Kd = 0
    
--     local temperature_desired = 50

--     local getValueSPI = receive()

--     local error = temperature_desired - getValueSPI
--     P = Kp * error
--     I = I + Ki * error
--     D = Kd * ( error - prev_error )
--     local PID = P + I + D
--     send(PID)

--     pid(I, error)
-- end

-- pid(0, 0)
