function compileTimeEvaluation(i)
    local c = 3
    local v = 1 + 2 + c
    local res = i + v
    compileTimeEvaluation(res)
end
compileTimeEvaluation(0)
