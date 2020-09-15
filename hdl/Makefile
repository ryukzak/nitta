PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
TEST := $(PWD)/test
CALL_TEST := $(shell which vvp) $(PWD)/test
GTKWAVE := $(shell which gtkwave)

clean:
	rm $(PWD)/test; rm -r $(PWD)/*.vcd

pu_accum:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_accum.v $(PWD)/pu_accum_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_accum_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_fram:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_fram.v $(PWD)/pu_fram_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_fram_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_mux:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_mux.v $(PWD)/pu_mux_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_mux_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_fifo:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_fifo.v $(PWD)/pu_fifo_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_fifo_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_shift:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_shift.v $(PWD)/pu_shift_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_shift_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_control:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_control.v $(PWD)/pu_control_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_control_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_div:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/div/pu_div.v $(PWD)/div/div_mock.v $(PWD)/div/pu_div_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_div_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_multiplier:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/multiplier/pu_multiplier.v $(PWD)/multiplier/mult_mock.v $(PWD)/multiplier/pu_multiplier_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_multiplier_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd
