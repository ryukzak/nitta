all: comp vvp #gtk

comp:
	iverilog -o puspi pu_spi.v pu_buffer.v pu_buffer_tb.v
vvp:
	vvp puspi 
gtk:
	gtkwave pu_spi_tb.vcd