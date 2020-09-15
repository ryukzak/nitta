icarus:
	iverilog ../../pu_accum.v ../../pu_fram.v ../../pu_shift.v ../../spi/spi_slave_driver.v ../../spi/spi_buffer.v ../../spi/spi_master_driver.v ../../spi/hoarder.v ../../spi/pu_slave_spi.v fibonacci_net/fibonacci_net.v ../../pu_simple_control.v fibonacci_net_tb.v
	vvp a.out
modelsim
	modelsim -do sim.do
