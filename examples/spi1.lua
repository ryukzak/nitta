function foo()
    local a = receive()
    local x = buffer(a)
    send(x)
    foo()
end
foo()
