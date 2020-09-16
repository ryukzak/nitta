PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
TEST := $(PWD)/test
CALL_TEST := $(shell which vvp) $(PWD)/test
GTKWAVE := $(shell which gtkwave)

clean:
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_multiplier:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_multiplier.v $(PWD)/mult_mock.v $(PWD)/pu_multiplier_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_multiplier_tb.vcd
