function shift(a)
	local out = a << 1
	debug.trace(out)
	shift(out)
end
shift(1)
