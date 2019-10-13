function pid(I, prev_err)
  local getValueSPI = receive()

  -- SPI model can send data only one time
  err = 50 - getValueSPI
  -- err = getValueSPI

  P = 2 * err
  I = (I + 0 * err)
  D = 0 * reg(err - prev_err)

  local PID = reg(P + I) + D
  send(PID)

  pid(I, err)
end

pid(0, 0)
