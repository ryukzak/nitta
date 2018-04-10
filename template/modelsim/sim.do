transcript on
if {[file exists rtl_work]} {
  vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

set path ""
append path [pwd] ""

$verilog_files$

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cycloneive_ver -L rtl_work -L work -voptargs="+acc" $top_level$

do wave.do

view structure
view signals
run 1200
view wave
