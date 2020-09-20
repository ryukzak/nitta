PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)

define test
	$(IVERILOG) -o $(PWD)/$(3) -I./ -y./ $(PWD)/$(1) $(PWD)/$(2)
	$(shell which vvp) $(PWD)/$(3)
	rm $(PWD)/$(3)
endef

all: pu_accum pu_fram pu_mux pu_fifo pu_shift pu_control

clean:
	rm -r $(PWD)/*.vcd

pu_accum:
	$(call test,pu_accum.v,pu_accum_tb.v,test_pu_accum_tb)

pu_fram:
	$(call test,pu_fram.v,pu_fram_tb.v,test_pu_fram_tb)

pu_mux:
	$(call test,pu_mux.v,pu_mux_tb.v,test_pu_mux_tb)

pu_fifo:
	$(call test,pu_fifo.v,pu_fifo_tb.v,test_pu_fifo_tb)

pu_shift:
	$(call test,pu_shift.v,pu_shift_tb.v,test_pu_shift_tb)

pu_control:
	$(call test,pu_control.v,pu_control_tb.v,test_pu_control_tb)
