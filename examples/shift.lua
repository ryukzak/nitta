function shift(a)
	local out = a << 1
	shift(out)
end
shift(1)
