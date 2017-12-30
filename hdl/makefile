all:	comp vvp gtk clean
	
comp: 
	iverilog -o test -I./ -y./ pu_accum.v pu_accum_tb.v

vvp: 
	vvp test

gtk:
	gtkwave pu_accum.vcd
