PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)

define test
	$(IVERILOG) -o $(PWD)/$(4) -I./ -y./ $(PWD)/$(1) $(PWD)/$(2) $(PWD)/$(3)
	$(shell which vvp) $(PWD)/$(4)
	rm $(PWD)/$(4)
endef

all: pu_multiplier

clean:
	rm -r $(PWD)/*.vcd

pu_multiplier:
	$(call test,pu_multiplier.v,mult_mock.v,pu_multiplier_tb.v,test_pu_multiplier_tb)
