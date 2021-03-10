function constantFolding(i)
    local v = 1 + 2 + 3
    local res = i + v + 3
    constantFolding(res)
end
constantFolding(0)
