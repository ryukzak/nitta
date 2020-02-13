function foo()
    local a = receive()
    local b = receive()
    local c = a + a + b
    send(c)
    foo()
end
foo()
