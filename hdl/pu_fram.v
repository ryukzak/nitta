module pu_fram
  #( parameter RAM_SIZE   = 16
   , parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter ADDR_WIDTH = $clog2( RAM_SIZE )
   , parameter FRAM_DUMP = "fram.dump"
   )
  ( input  wire                  clk

  , input  wire [ADDR_WIDTH-1:0] signal_addr
  , input  wire                  signal_wr
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out
  );


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
initial $readmemh(FRAM_DUMP, bank, 0, RAM_SIZE-1);

always @(posedge clk)
  if ( wr ) bank[addr] <= { attr, data };

always @(posedge clk)
  if ( ~signal_oe ) { attr_out, data_out } <= 0;
  else              { attr_out, data_out } <= bank[ signal_addr ];

endmodule
