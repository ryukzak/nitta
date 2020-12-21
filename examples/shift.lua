function shift(a)
	local out = a << 9
	shift(out)
end
shift(1)
