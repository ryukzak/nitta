PWD := $(shell pwd)
IVERILOG := $(shell which iverilog)

define test
	$(IVERILOG) -o $(PWD)/$(4) -I./ -y./ $(PWD)/$(1) $(PWD)/$(2) $(PWD)/$(3)
	$(shell which vvp) $(PWD)/$(4)
	rm $(PWD)/$(4)
endef

all: pu_div

clean:
	rm -r $(PWD)/*.vcd

pu_div:
	$(call test,pu_div.v,div_mock.v,pu_div_tb.v,test_pu_div_tb)
