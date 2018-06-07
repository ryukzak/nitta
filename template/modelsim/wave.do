onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /$top_level$/clk
add wave -noupdate /$top_level$/rst
add wave -noupdate -radix decimal /$top_level$/net/cycle
add wave -noupdate -radix hexadecimal /$top_level$/net/control_bus
add wave -noupdate -radix decimal /$top_level$/net/data_bus
add wave -noupdate /$top_level$/net/attr_bus
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {0 ps} 0}
quietly wave cursor active 0
configure wave -namecolwidth 318
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
WaveRestoreZoom {0 ps} {828 ps}
