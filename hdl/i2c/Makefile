PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)

define test
	$(IVERILOG) -o $(PWD)/$(3) -I./ -y./ $(PWD)/$(1) $(PWD)/$(2)
	$(shell which vvp) $(PWD)/$(3)
	rm $(PWD)/$(3)
endef

all: buffer bounce_filter i2c nitta_to_i2c_splitter pu_i2c_master_driver

clean:
	rm -r $(PWD)/*.vcd

buffer:
	$(call test,buffer.v,buffer_tb.v,test_buffer_tb)

bounce_filter:
	$(call test,bounce_filter.v,bounce_filter_tb.v,test_bounce_filter_tb)

i2c:
	$(call test,i2c_master_driver.v,i2c_tb.v,test_i2c_tb)

nitta_to_i2c_splitter:
	$(call test,nitta_to_i2c_splitter_tb.v,i2c_tb.v,test_nitta_to_i2c_splitter_tb)

pu_i2c_master_driver:
	$(call test,pu_i2c_master_driver.v,pu_i2c_master_driver_tb.v,test_pu_i2c_master_driver_tb)
