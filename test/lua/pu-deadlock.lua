function pid(I, prev_err)
  err = 50 - 2

  I = I + 1 * err
  D = 0 * (err - prev_err)

  pid(I, I + D)
end

pid(0, 0)
