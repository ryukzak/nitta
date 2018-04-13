module nitta
  ( input external_clk

  , input [3:0] dips
  , input [1:0] keys
  , output [7:0] leds

  , input mosi
  , output miso
  , input sclk
  , input cs
  ); 
 
wire clk_200MHz, clk_5kHz, clk_1Hz;

wire rst           = dips[3];
wire boost         = dips[2];
wire rendezvous    = dips[1];
wire show_bus_7_0  = !keys[0];
wire show_bus_15_8 = !keys[1];

pll pll
  ( .inclk0( external_clk )
  , .c0( clk_200MHz )
  , .c1( clk_5kHz )
  );

reg [ 15:0 ] counter;
always @( posedge clk_5kHz )
  if ( rst || counter >= 5000 ) counter <= 0;
  else                          counter <= counter + 1;
  
assign clk_1Hz = counter <= 2500;

wire [7:0] debug_status, debug_bus1, debug_bus2;

assign leds = show_bus_7_0 ? debug_bus1
            : show_bus_15_8 ? debug_bus2
            : debug_status;

$top_level_module$ 
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 4 )
   ) net 
  ( .rst( rst )
  , .clk( boost ? clk_200MHz : clk_1Hz )
  , .mosi( mosi ), .sclk( sclk ), .cs( cs ), .miso( miso )
  , .debug_status( debug_status )
  , .debug_bus1( debug_bus1 )
  , .debug_bus2( debug_bus2 )
  , .is_drop_allow( rendezvous )
  ); 
               
endmodule