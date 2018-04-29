`timescale 1 ms/ 1 ms
module spi_buffer
  #( parameter DATA_WIDTH     = 32
   , parameter BUF_SIZE       = 10
   // FIXME: , parameter ATTR_WIDTH     = 4
   )
  ( input                       clk
  , input                       rst

  , input                       wr
  , input      [DATA_WIDTH-1:0] data_in

  , input                       oe 
  , output     [DATA_WIDTH-1:0] data_out
  );

localparam ADDR_WIDTH  = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1]; 
reg [ADDR_WIDTH-1:0] addr;

always @( posedge clk ) begin
  if ( rst ) begin
    addr <= 0;
  end else if ( wr ) begin
    memory[ addr ] <= data_in;      
    addr <= addr + 1;
  end else if ( oe ) begin
    addr <= addr + 1;
  end
end

assign data_out = memory[ addr ];

endmodule