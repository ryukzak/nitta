`timescale 1 ms/ 1 ms
module buffer
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4  // FIXME:
   , parameter BUF_SIZE   = 10
   )
  ( input                   clk
  , input                   rst

  , input                   wr
  , input  [DATA_WIDTH-1:0] data_in

  , input                   oe 
  , output reg [DATA_WIDTH-1:0] data_out
  );

localparam ADDR_WIDTH = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1]; 
reg [ADDR_WIDTH-1:0] addr;

reg [DATA_WIDTH-1:0] future;
reg future_flag; 

always @( posedge clk ) begin
  if ( rst ) begin
    addr <= 0;
    data_out <= 0 ;    
  end else if ( wr | oe ) begin
    if ( wr ) memory[ addr ] <= data_in;
    if ( oe ) data_out <= memory[ addr ] ;
    addr <= addr + 1;
  end
end

endmodule
