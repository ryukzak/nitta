PWD := $(shell pwd)

all:	comp vvp gtk clean

comp:
	iverilog -o test -I./ -y./ $(PWD)/pu_accum.v $(PWD)/pu_accum_tb.v

vvp:
	vvp test

gtk:
	$(PWD)/test;gtkwave $(PWD)/pu_accum_tb.vcd
clean:
	rm $(PWD)/test
