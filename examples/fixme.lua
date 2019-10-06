function pid(I, prev_err)
  local getValueSPI = receive()

  err = 50 - getValueSPI

  P = 2 * err
  D = 0 * reg(err - prev_err)

  local PID = P + I + D

  pid(PID, err)
end

pid(0, 0)
