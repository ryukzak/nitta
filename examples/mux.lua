function muxf(a, b)
    local cond = 1
    local res = if_mux (cond, a, b)
    muxf(res, a)
end

muxf(1, 0)
