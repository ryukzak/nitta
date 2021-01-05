function counter(x)
    send(x)
    x = x + 1
    counter(x)
end
counter(0)
