create_clock -period 50MHz -name {external_clk} [get_ports {external_clk}]
derive_pll_clocks
derive_clock_uncertainty

set_input_delay -clock external_clk -max 0.5 [all_inputs]
set_input_delay -clock external_clk -min 0.1 [all_inputs]

set_output_delay -clock external_clk -max 0.5 [all_outputs]
set_output_delay -clock external_clk -min 0.1 [all_outputs] 
