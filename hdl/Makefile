PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
MAKE := $(shell which make)
RM := $(shell which rm) -f

define test
	$(IVERILOG) -o $(PWD)/$(3).out -I./ -y./ $(PWD)/$(1) $(PWD)/$(2)
	$(shell which vvp) $(PWD)/$(3).out
endef

all: pu_accum pu_fram pu_fifo pu_shift pu_div pu_multiplier pu_i2c pu_spi pu_compare pu_multiplexer #pu_lut

clean:
	$(RM) -r $(PWD)/*.vcd $(PWD)/*.out
	$(MAKE) clean -C $(PWD)/i2c
	$(MAKE) clean -C $(PWD)/div
	$(MAKE) clean -C $(PWD)/multiplier
	$(MAKE) clean -C $(PWD)/spi

pu_accum:
	$(call test,pu_accum.v,pu_accum_tb.v,test_pu_accum_tb)

pu_fram:
	$(call test,pu_fram.v,pu_fram_tb.v,test_pu_fram_tb)

pu_multiplexer:
	$(call test,pu_multiplexer.v,pu_multiplexer_tb.v,test_pu_multiplexer_tb)

pu_fifo:
	$(call test,pu_fifo.v,pu_fifo_tb.v,test_pu_fifo_tb)

pu_shift:
	$(call test,pu_shift.v,pu_shift_tb.v,test_pu_shift_tb)

pu_lut:
	$(call test,pu_lut.v,pu_lut_tb.v,test_pu_lut_tb)

pu_compare:
	$(call test,pu_compare.v,pu_compare_tb.v,test_pu_compare_tb)

pu_div:
	$(MAKE) -C $(PWD)/div

pu_multiplier:
	$(MAKE) -C $(PWD)/multiplier

pu_i2c:
	$(MAKE) -C $(PWD)/i2c

pu_spi:
	$(MAKE) -C $(PWD)/spi
