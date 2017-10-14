module pu_fram(
  clk,

  signal_addr,
  signal_wr,
  signal_oe,
  
  data_in,
  attr_in,
  data_out,
  attr_out
);

parameter RAM_SIZE   = 16;
parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;
parameter ADDR_WIDTH = $clog2( RAM_SIZE );

input                   clk;
input  [ADDR_WIDTH-1:0] signal_addr;
input                   signal_wr;
input                   signal_oe;
input  [DATA_WIDTH-1:0] data_in;
input  [ATTR_WIDTH-1:0] attr_in;
output [DATA_WIDTH-1:0] data_out;
output [ATTR_WIDTH-1:0] attr_out;

reg [DATA_WIDTH-1:0] data;
reg [ATTR_WIDTH-1:0] attr;
reg [ADDR_WIDTH-1:0] addr;
reg                  wr;
   
always @(posedge clk)
  begin
    addr <= signal_addr;
    wr   <= signal_wr;
    data <= data_in;
    attr <= attr_in;
  end

reg [DATA_WIDTH+ATTR_WIDTH-1:0] bank [RAM_SIZE-1:0];
always @(posedge clk)
  if ( wr ) bank[addr] <= { attr_in, data_in };

reg [DATA_WIDTH-1:0] data_out;
reg [ATTR_WIDTH-1:0] attr_out;
always @(posedge clk)
  if ( ~signal_oe ) { attr_out, data_out } <= 0;
  else              { attr_out, data_out } <= bank[ signal_addr ];

endmodule
