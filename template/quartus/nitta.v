module nitta
  ( input clk
  , input rst
  , input mosi
  , output miso
  , input sclk
  , input cs
  ); 
 
accum_fram1_fram2_shift_spi_net net 
  ( .clk( clk ) 
  , .rst( rst ) 
  , .mosi( mosi ), .sclk( sclk ), .cs( cs ), .miso( miso )
  ); 
 
endmodule