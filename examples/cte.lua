function cte(i)
    local c = 3
    local v = 1 + 2 + c
    local res = i + v
    cte(res)
end
cte(0)
