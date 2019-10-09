function pid(I, prev_err)
  err = 50

  P = 2 * err
  I = (I + 0 * err)
  D = 0 * (err - prev_err)
  -- D = 0 * reg(err - prev_err)

  send(P + I + D)

  pid(I, err)
end

pid(0, 0)
