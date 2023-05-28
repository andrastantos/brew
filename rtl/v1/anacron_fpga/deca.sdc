create_clock -name ADC_CLK_10 -period 100.00 [get_ports {ADC_CLK_10}]
create_clock -name MAX10_CLK1_50 -period 20.00 [get_ports {MAX10_CLK1_50}]
create_clock -name clk -period 100.00 [get_ports {clk}]
create_clock -name clk2 -period 20.00 [get_ports {clk2}]

derive_clock_uncertainty
