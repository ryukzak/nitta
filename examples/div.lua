function div(a, b)
    local x = a / b
    div(x, b)
end
div(-25, 2)