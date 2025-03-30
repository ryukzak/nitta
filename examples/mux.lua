function muxf(a, b)
    local c = a and b
    local r = b or b
    local cond = c == r
    local res = mux (cond, a, b)
    muxf(a, res)
end

muxf(1, 0)
