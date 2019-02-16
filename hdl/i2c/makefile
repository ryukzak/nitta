CC          := iverilog
CFLAGS      := -Wall
FILE        := $(filter-out $(wildcard *_tb*.v), $(wildcard *.v))

all: i2c_tb

unix:
	iverilog $(CFLAGS) $(FILE) && ./a.out

i2c_tb: i2c_tb.out
	vvp i2c_tb.out

i2c_tb.out: $(FILE) i2c_tb.v
	$(CC) -o i2c_tb.out $^