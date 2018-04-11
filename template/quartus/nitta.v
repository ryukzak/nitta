module nitta
  ( input external_clk
  , input rst
  , input mosi
  , output miso
  , input sclk
  , input cs
  ); 
 
wire clk;

pll pll
  ( .inclk0( external_clk )
  , .c0(clk)
  );
  
$top_level_module$ net 
  ( .clk( clk ) 
  , .rst( rst ) 
  , .mosi( mosi ), .sclk( sclk ), .cs( cs ), .miso( miso )
  ); 
 
endmodule