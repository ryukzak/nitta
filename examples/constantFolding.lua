function constantFolding(i)
    local c = 3
    local v = 1 + 2 + c
    local res = i + v
    constantFolding(res)
end
constantFolding(0)
