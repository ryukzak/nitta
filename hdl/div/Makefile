PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
TEST := $(PWD)/test
CALL_TEST := $(shell which vvp) $(PWD)/test
GTKWAVE := $(shell which gtkwave)

clean:
	rm $(TEST); rm -r $(PWD)/*.vcd

pu_div:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_div.v $(PWD)/div_mock.v $(PWD)/pu_div_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_div_tb.vcd
