transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -vlog01compat -work work +incdir+C:/nitta/hdl/mult {C:/nitta/hdl/mult/pu_div.v}
vlog -vlog01compat -work work +incdir+C:/nitta/hdl/mult {C:/nitta/hdl/mult/play.v}
vlog -vlog01compat -work work +incdir+C:/nitta/hdl/mult {C:/nitta/hdl/mult/div.v}
vlog -vlog01compat -work work +incdir+C:/nitta/hdl/mult {C:/nitta/hdl/mult/pll.v}
vlog -vlog01compat -work work +incdir+C:/nitta/hdl/mult/db {C:/nitta/hdl/mult/db/pll_altpll.v}

vlog -vlog01compat -work work +incdir+C:/nitta/hdl/mult {C:/nitta/hdl/mult/pu_div_tb.v}

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cycloneive_ver -L rtl_work -L work -voptargs="+acc"  pu_div_tb


do wave.do



view structure
run -all
view signals
