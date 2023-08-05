set pll_freq 100
puts "PLL frequency is $pll_freq"
set_property -dict [list CONFIG.CLKOUT1_REQUESTED_OUT_FREQ $pll_freq] [get_ips clk_pll]
