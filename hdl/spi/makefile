CC          := iverilog
CFLAGS      := -Wall
FILE        := $(filter-out $(wildcard *_tb*.v), $(wildcard *.v))
PWD         := $(shell pwd)

all: buffer_tb                \
	 bounce_filter_tb         \
	 nitta_to_spi_splitter_tb \
	 pu_slave_spi_tb          \
	 pu_slave_spi_tb
	 #Etc.

unix:
	iverilog $(CFLAGS) $(FILE) && ./a.out

testbenchs: nitta_to_spi_splitter_tb buffer_tb spi_slave_driver_tb

buffer_tb: buffer_tb.out
	vvp buffer_tb.out

buffer_tb.out: $(FILE) buffer_tb.v
	$(CC) -o buffer_tb.out $^

bounce_filter_tb: bounce_filter.out
	vvp bounce_filter.out

bounce_filter.out: $(FILE) bounce_filter_tb.v
	$(CC) -o bounce_filter.out $^

pu_slave_spi_tb: pu_slave_spi_tb.out
	vvp pu_slave_spi_tb.out

pu_slave_spi_tb.out: $(FILE) pu_slave_spi_tb.v
	$(CC) -o pu_slave_spi_tb.out $^

nitta_to_spi_splitter_tb: nitta_to_spi_splitter_tb.out
	vvp nitta_to_spi_splitter_tb.out

nitta_to_spi_splitter_tb.out: $(FILE) nitta_to_spi_splitter_tb.v
	$(CC) -o nitta_to_spi_splitter_tb.out $^

spi_slave_driver_tb: spi_slave_driver_tb.out
	vvp spi_slave_driver_tb.out

spi_slave_driver_tb.out: $(FILE) spi_slave_driver_tb.v
	$(CC) -o spi_slave_driver_tb.out $^

spi_slave_driver_tb.1: spi_slave_driver_tb.1.out
	vvp spi_slave_driver_tb.1.out

spi_slave_driver_tb.1.out: $(FILE) spi_slave_driver_tb.1.v
	$(CC) -o spi_slave_driver_tb.1.out $^

pu_spi_slave_driver: $(FILE) pu_spi_slave_driver_tb.v
	$(CC) -o pu_spi_slave_driver.out $^ && vvp.exe pu_spi_slave_driver.out

pu_master_spi_tb: pu_master_spi_tb.out
	vvp pu_master_spi_tb.out

pu_master_spi_tb.out: $(FILE) pu_master_spi_tb.v
	$(CC) -o pu_master_spi_tb.out $^
clean:
	rm -r $(PWD)/*.vcd; rm -rf $(PWD)/*.out
