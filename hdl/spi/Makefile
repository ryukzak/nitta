PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
RM := $(shell which rm) -f

define test
	$(IVERILOG) -o $(PWD)/$(3).out -I./ -y./ $(PWD)/$(1) $(PWD)/$(2)
	$(shell which vvp) $(PWD)/$(3).out
endef

all: bounce_filter_tb pu_slave_spi_driver_tb pu_master_spi_tb spi_slave_driver_tb

clean:
	$(RM) -r $(PWD)/*.vcd $(PWD)/*.out

bounce_filter_tb:
	$(call test,bounce_filter.v,bounce_filter_tb.v,test_bounce_filter_tb)

pu_slave_spi_driver_tb:
	$(call test,pu_slave_spi_driver.v,pu_slave_spi_driver_tb.v,test_pu_slave_spi_driver)

pu_master_spi_tb:
	$(call test,pu_master_spi.v,pu_master_spi_tb.v,test_pu_master_spi_tb)

spi_slave_driver_tb:
	$(call test,spi_slave_driver.v,spi_slave_driver_tb.v,testspi_slave_driver_tb)
