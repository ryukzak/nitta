create_clock -period 50MHz -name {CLOCK_50} [get_ports {CLOCK_50}]

derive_pll_clocks
derive_clock_uncertainty

set_false_path -from [get_ports {RST}]
set_false_path -from [get_ports {MODE}]
set_false_path -from [get_ports {SPEED}]
set_false_path -to [get_ports {LED[*]}]
# А почему нет вопроса про выходы GPIO, а про LED есть?
# Как он нашёл этот дополнительный clk? Почему он clk?