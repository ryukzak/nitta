onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /{{ nitta.testbench.module_name }}/clk
add wave -noupdate /{{ nitta.testbench.module_name }}/rst
add wave -noupdate -radix decimal /{{ nitta.testbench.module_name }}/net/cycle
add wave -noupdate -radix hexadecimal /{{ nitta.testbench.module_name }}/net/control_bus
add wave -noupdate -radix decimal /{{ nitta.testbench.module_name }}/net/data_bus
add wave -noupdate /{{ nitta.testbench.module_name }}/net/attr_bus
TreeUpdate [SetDefaultTree]

WaveRestoreCursors {{ "{{" }}Cursor 1} {0 ps} 0}

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
