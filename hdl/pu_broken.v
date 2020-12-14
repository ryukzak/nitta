module pu_broken
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter IS_BROKEN = 0
   , parameter WRONG_ATTR = 0
   )
  ( input  wire                  clk

  , input  wire                  signal_wr
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out
  );

reg [DATA_WIDTH-1:0] data_buf;
reg [ATTR_WIDTH-1:0] attr_buf;

always @(posedge clk)
  if ( signal_wr ) begin
    data_buf <= data_in + IS_BROKEN;
    attr_buf <= { WRONG_ATTR ? ~attr_in : attr_in };
  end

always @(posedge clk)
  if ( signal_oe ) begin
    data_out <= data_buf;
    attr_out <= attr_buf;
  end else begin
    { attr_out, data_out } <= 0;
  end

endmodule
