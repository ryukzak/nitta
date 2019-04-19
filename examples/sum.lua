function sum()
    local a = receive()
    local b = receive()
    local c = a + b
    send(c)
    sum()
end
sum()
