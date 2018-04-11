transcript on
if {[file exists rtl_work]} {
  vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

set path ""
append path [pwd] ""

vlog -vlog01compat -work work +incdir+$path $path/../../pu_accum.v
vlog -vlog01compat -work work +incdir+$path $path/../../pu_fram.v
vlog -vlog01compat -work work +incdir+$path $path/../../pu_shift.v
vlog -vlog01compat -work work +incdir+$path $path/../../spi/spi_slave_driver.v
vlog -vlog01compat -work work +incdir+$path $path/../../spi/spi_buffer.v
vlog -vlog01compat -work work +incdir+$path $path/../../spi/spi_master_driver.v
vlog -vlog01compat -work work +incdir+$path $path/../../spi/hoarder.v
vlog -vlog01compat -work work +incdir+$path $path/../../spi/pu_slave_spi.v
vlog -vlog01compat -work work +incdir+$path $path/fibonacci_net/fibonacci_net.v
vlog -vlog01compat -work work +incdir+$path $path/../../pu_simple_control.v
vlog -vlog01compat -work work +incdir+$path $path/fibonacci_net_tb.v

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cycloneive_ver -L rtl_work -L work -voptargs="+acc" fibonacci_net_tb

do wave.do

view structure
view signals
run 1200
view wave