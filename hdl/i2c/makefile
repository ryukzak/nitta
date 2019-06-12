CC          := iverilog
CFLAGS      := -Wall
FILE        := $(filter-out $(wildcard *_tb*.v), $(wildcard *.v))

all: pu_i2c_master_driver_tb

unix:
	iverilog $(CFLAGS) $(FILE) && ./a.out

testbenchs: nitta_to_i2c_splitter_tb buffer_tb i2c_slave_driver_tb

buffer_tb: buffer_tb.out
	vvp buffer_tb.out

buffer_tb.out: $(FILE) buffer_tb.v
	$(CC) -o buffer_tb.out $^

bounce_filter_tb: bounce_filter.out
	vvp bounce_filter.out

bounce_filter.out: $(FILE) bounce_filter_tb.v
	$(CC) -o bounce_filter.out $^

nitta_to_i2c_splitter_tb: nitta_to_i2c_splitter_tb.out
	vvp nitta_to_i2c_splitter_tb.out

nitta_to_i2c_splitter_tb.out: $(FILE) nitta_to_i2c_splitter_tb.v
	$(CC) -o nitta_to_i2c_splitter_tb.out $^

pu_i2c_master_driver_tb: pu_i2c_master_driver_tb.out
	vvp pu_i2c_master_driver_tb.out

pu_i2c_master_driver_tb.out: $(FILE) pu_i2c_master_driver_tb.v
	$(CC) -o pu_i2c_master_driver_tb.out $^