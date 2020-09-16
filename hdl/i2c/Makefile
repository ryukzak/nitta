PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
TEST := $(PWD)/test
CALL_TEST := $(shell which vvp) $(PWD)/test
GTKWAVE := $(shell which gtkwave)

clean:
	rm $(TEST); rm -r $(PWD)/*.vcd

buffer:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/buffer.v $(PWD)/buffer_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/buffer_tb.vcd

bounce_filter:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/bounce_filter.v $(PWD)/bounce_filter_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/bounce_filter_tb.vcd

i2c:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/i2c_master_driver.v $(PWD)/i2c_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/i2c_tb.vcd
	rm $(TEST); rm -r $(PWD)/*.vcd

nitta_to_i2c_splitter:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/nitta_to_i2c_splitter.v $(PWD)/nitta_to_i2c_splitter_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/nitta_to_i2c_splitter_tb.vcd

pu_i2c_master_driver:
	$(IVERILOG) -o $(TEST) -I./ -y./ $(PWD)/pu_i2c_master_driver.v $(PWD)/pu_i2c_master_driver_tb.v
	$(CALL_TEST); $(GTKWAVE) $(PWD)/pu_i2c_master_driver_tb.vcd
