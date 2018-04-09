onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /pu_mult_tb/clk
add wave -noupdate /pu_mult_tb/rst
add wave -noupdate /pu_mult_tb/signal_sel
add wave -noupdate /pu_mult_tb/signal_wr
add wave -noupdate -radix decimal /pu_mult_tb/data_in
add wave -noupdate -radix hexadecimal /pu_mult_tb/attr_in
add wave -noupdate /pu_mult_tb/signal_oe
add wave -noupdate -radix decimal /pu_mult_tb/data_out
add wave -noupdate -radix hexadecimal /pu_mult_tb/attr_out
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {0 ps} 0}
quietly wave cursor active 0
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
WaveRestoreZoom {580 ps} {1580 ps}
