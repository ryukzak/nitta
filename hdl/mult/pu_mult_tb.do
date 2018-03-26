transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

set path ""
append path [pwd] "/../.."

vlog -vlog01compat -work work +incdir+$path $path/play.v
vlog -vlog01compat -work work +incdir+$path $path/pu_mult.v
vlog -vlog01compat -work work +incdir+$path $path/pll.v
vlog -vlog01compat -work work +incdir+$path/db $path/db/pll_altpll.v
vlog -vlog01compat -work work +incdir+$path $path/mult_inner.v

vlog -vlog01compat -work work +incdir+$path $path/pu_mult_tb.v

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cycloneive_ver -L rtl_work -L work -voptargs="+acc"  pu_mult_tb

do ../../wave.do

view structure
view signals
run 1200
view wave
