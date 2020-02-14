function sum()
    local a = receive()
    local x = a + a
    send(x)
    sum()
end
sum()
