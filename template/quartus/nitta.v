module nitta
  ( input clk
  , input rst
  , input mosi
  , output miso
  , input sclk
  , input cs
  ); 
 
$top_level_module$ net 
  ( .clk( clk ) 
  , .rst( rst ) 
  , .mosi( mosi ), .sclk( sclk ), .cs( cs ), .miso( miso )
  ); 
 
endmodule