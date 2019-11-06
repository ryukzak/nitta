function acc(x)
    local y = x - 1
    acc(y)
end
acc(100)
