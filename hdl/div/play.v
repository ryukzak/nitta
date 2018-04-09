module play (
  input CLOCK_50,
  // input clk,
  input RST, // dip 1
  input MODE, // dip 2, 0 - ext, 1 - int
  input SPEED, // key 0 - speed up
  input SEL, // dip3; 0 - direct, 1 - reverse 
  output [7:0] LED,

  output ext_master_mosi,
  input  ext_master_miso,
  output ext_master_sclk,
  output ext_master_cs,
  
  input  ext_slave_mosi,
  output ext_slave_miso,
  input  ext_slave_sclk,
  input  ext_slave_cs
  );

parameter DATA_WIDTH     = 32; 
parameter ATTR_WIDTH     = 4;  
parameter INVALID     = 1;  

wire clk;
pll pll(
  .inclk0(CLOCK_50),
  .c0(clk)
  );
  
  
reg [31:0] counter_data;
always @(posedge clk) begin
   if (RST) counter_data <= 0;
   else counter_data <= counter_data + 1;
end


wire [31:0] data_out;
wire [3:0] attr_out;

pu_div 
  #( .DATA_WIDTH( DATA_WIDTH )
   , .ATTR_WIDTH( ATTR_WIDTH )
   , .INVALID( INVALID )
   ) unit 
  ( .clk( clk )
  , .rst( RST )
  , .signal_wr( MODE )
  , .signal_sel( SPEED )
  , .data_in( counter_data )
  , .attr_in( counter_data[3:0] )
  , .res_select( SEL )
  , .signal_oe( SEL )
  , .data_out( data_out )
  , .attr_out( attr_out )
  );

assign LED = data_out[31:24];

  
endmodule
