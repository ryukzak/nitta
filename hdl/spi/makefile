FILE = spi_master_driver.v spi_slave_driver.v pu_slave_spi.v spi_buffer.v pu_slave_spi_tb.v

all: comp vvp #gtk

comp:
	iverilog -o puspi $(FILE)
vvp:
	vvp puspi 
gtk:
	gtkwave pu_slave_spi_tb.vcd