FILE = spi_master_driver.v spi_slave_driver.v pu_slave_spi_tb.v pu_slave_spi.v buffer.v nitta_to_spi_splitter.v

unix:
	iverilog -Wall $(FILE) && ./a.out

all: comp vvp #gtk

comp:
	iverilog -o puspi $(FILE)
vvp:
	vvp puspi
gtk:
	gtkwave pu_slave_spi_tb.vcd


testbenchs: nitta_to_spi_splitter_tb buffer_tb spi_slave_driver_tb

nitta_to_spi_splitter_tb: nitta_to_spi_splitter_tb.out
	vvp nitta_to_spi_splitter_tb.out

nitta_to_spi_splitter_tb.out: nitta_to_spi_splitter.v nitta_to_spi_splitter_tb.v
	iverilog -o nitta_to_spi_splitter_tb.out nitta_to_spi_splitter.v nitta_to_spi_splitter_tb.v


buffer_tb: buffer_tb.out
	vvp buffer_tb.out

buffer_tb.out: buffer.v buffer_tb.v
	iverilog -o buffer_tb.out buffer.v buffer_tb.v


spi_slave_driver_tb: spi_slave_driver_tb.out
	vvp spi_slave_driver_tb.out

spi_slave_driver_tb.out: spi_slave_driver.v spi_slave_driver_tb.v
	iverilog -o spi_slave_driver_tb.out spi_slave_driver.v spi_slave_driver_tb.v
