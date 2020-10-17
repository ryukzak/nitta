PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
RM := $(shell which rm) -f

define test
	$(IVERILOG) -o $(PWD)/$(3).out -I./ -y./ $(PWD)/$(1) $(PWD)/$(2)
	$(shell which vvp) $(PWD)/$(3).out
endef

all: buffer bounce_filter i2c pu_i2c_master_driver

clean:
	$(RM) -r $(PWD)/*.vcd $(PWD)/*.out

buffer:
	$(call test,buffer.v,buffer_tb.v,test_buffer_tb)

bounce_filter:
	$(call test,bounce_filter.v,bounce_filter_tb.v,test_bounce_filter_tb)

i2c:
	$(call test,i2c_master_driver.v,i2c_tb.v,test_i2c_tb)

pu_i2c_master_driver:
	$(call test,pu_i2c_master_driver.v,pu_i2c_master_driver_tb.v,test_pu_i2c_master_driver_tb)
