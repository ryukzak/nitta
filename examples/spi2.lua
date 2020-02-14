function foo()
    local a = receive()
    local b = receive()
    local c = a + b
    send(c)
    foo()
end
foo()
