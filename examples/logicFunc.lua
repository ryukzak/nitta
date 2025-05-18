function logicf(a, b)
    local r1 = a and b
    local r2 = a or r1
    local r3 = not r2
    logicf(r3, r1)
end

logicf(1, 0)
