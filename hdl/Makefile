PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)

clean:
	rm $(PWD)/test; rm -r $(PWD)/*.vcd

pu_accum:
	$(IVERILOG) -o test -I./ -y./ $(PWD)/pu_accum.v $(PWD)/pu_accum_tb.v
	vvp test
	$(PWD)/test;gtkwave $(PWD)/pu_accum_tb.vcd
	rm $(PWD)/test; rm -r $(PWD)/*.vcd

pu_fram:
	$(IVERILOG) -o test -I./ -y./ $(PWD)/pu_fram.v $(PWD)/pu_fram_tb.v
	vvp test
	$(PWD)/test;gtkwave $(PWD)/pu_fram_tb.vcd
	rm $(PWD)/test; rm -r $(PWD)/*.vcd

pu_mux:
	$(IVERILOG) -o test -I./ -y./ $(PWD)/pu_mux.v $(PWD)/pu_mux_tb.v
	vvp test
	$(PWD)/test;gtkwave $(PWD)/pu_mux_tb.vcd
	rm $(PWD)/test; rm -r $(PWD)/*.vcd

pu_fifo:
	$(IVERILOG) -o test -I./ -y./ $(PWD)/pu_fifo.v $(PWD)/pu_fifo_tb.v
	vvp test
	$(PWD)/test;gtkwave $(PWD)/pu_fifo_tb.vcd
	rm $(PWD)/test; rm -r $(PWD)/*.vcd

pu_shift:
	$(IVERILOG) -o test -I./ -y./ $(PWD)/pu_shift.v $(PWD)/pu_shift_tb.v
	vvp test
	$(PWD)/test;gtkwave $(PWD)/pu_shift_tb.vcd
	rm $(PWD)/test; rm -r $(PWD)/*.vcd
