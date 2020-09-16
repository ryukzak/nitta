PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
TEST := $(PWD)/test
CALL_TEST := $(shell which vvp) $(PWD)/test
GTKWAVE := $(shell which gtkwave)

clean:
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_accum:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_accum.v $(PWD)/pu_accum_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_accum_tb.vcd

pu_fram:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_fram.v $(PWD)/pu_fram_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_fram_tb.vcd

pu_mux:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_mux.v $(PWD)/pu_mux_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_mux_tb.vcd

pu_fifo:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_fifo.v $(PWD)/pu_fifo_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_fifo_tb.vcd

pu_shift:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_shift.v $(PWD)/pu_shift_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_shift_tb.vcd

pu_control:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_control.v $(PWD)/pu_control_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_control_tb.vcd
