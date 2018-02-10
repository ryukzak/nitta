transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -vlog01compat -work work +incdir+//Mac/Home/Documents/src/nitta/hdl/mult {//Mac/Home/Documents/src/nitta/hdl/mult/play.v}
vlog -vlog01compat -work work +incdir+//Mac/Home/Documents/src/nitta/hdl/mult {//Mac/Home/Documents/src/nitta/hdl/mult/pll.v}
vlog -vlog01compat -work work +incdir+//Mac/Home/Documents/src/nitta/hdl/mult {//Mac/Home/Documents/src/nitta/hdl/mult/pu_mult.v}
vlog -vlog01compat -work work +incdir+//Mac/Home/Documents/src/nitta/hdl/mult {//Mac/Home/Documents/src/nitta/hdl/mult/mult_inner.v}
vlog -vlog01compat -work work +incdir+//Mac/Home/Documents/src/nitta/hdl/mult/db {//Mac/Home/Documents/src/nitta/hdl/mult/db/pll_altpll.v}

vlog -vlog01compat -work work +incdir+/Documents/src/nitta/hdl/mult {/Documents/src/nitta/hdl/mult/pu_mult_tb.v}

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cycloneive_ver -L rtl_work -L work -voptargs="+acc"  pu_mult_tb

do wave.do

view structure
view signals
run 500

view wave
