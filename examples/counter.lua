function counter(x1)
    send(x1)
    x2 = x1 + 1
    counter(x2)
end
counter(0)
