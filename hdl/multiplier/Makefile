PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)
RM := $(shell which rm) -f

define test
	$(IVERILOG) -o $(PWD)/$(4).out -I./ -y./ $(PWD)/$(1) $(PWD)/$(2) $(PWD)/$(3)
	$(shell which vvp) $(PWD)/$(4).out
endef

all: pu_multiplier

clean:
	$(RM) -r $(PWD)/*.vcd $(PWD)/*.out

pu_multiplier:
	$(call test,pu_multiplier.v,mult_mock.v,pu_multiplier_tb.v,test_pu_multiplier_tb)
