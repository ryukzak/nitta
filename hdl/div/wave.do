onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -radix decimal /pu_div_tb/data_in
add wave -noupdate /pu_div_tb/signal_oe
add wave -noupdate /pu_div_tb/rst
add wave -noupdate /pu_div_tb/signal_sel
add wave -noupdate /pu_div_tb/res_select
add wave -noupdate /pu_div_tb/signal_wr
add wave -noupdate /pu_div_tb/clk
add wave -noupdate /pu_div_tb/attr_in
add wave -noupdate -radix decimal /pu_div_tb/data_out
add wave -noupdate -radix unsigned /pu_div_tb/attr_out
add wave -noupdate -radix decimal /pu_div_tb/i1/count
add wave -noupdate -radix decimal /pu_div_tb/i1/quotient_result
add wave -noupdate -radix decimal /pu_div_tb/i1/remain_result
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {770 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 150
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ps} {1 ns}
