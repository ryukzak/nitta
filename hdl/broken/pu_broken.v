module pu_broken
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter IS_BROKEN = 0
   )
  ( input  wire                  clk

  , input  wire                  signal_wr
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out
  );

reg [DATA_WIDTH+ATTR_WIDTH-1:0] bank;
   
always @(posedge clk)
  if ( signal_wr ) bank <= { attr_in, data_in };

always @(posedge clk)
  if ( signal_oe ) { attr_out, data_out } <= bank + IS_BROKEN;
  else             { attr_out, data_out } <= 0;

endmodule
