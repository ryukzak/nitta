function foo()
    local a = receive()
    local x = reg(a)
    send(x)
    foo()
end
foo()
