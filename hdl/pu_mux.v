// `timescale 1ns / 1ps
module pu_mux
  #( parameter DATA_WIDTH = 32
  ,  parameter ATTR_WIDTH = 4
  ,  parameter MUX_SIZE   = 4
  ,  parameter SEL_WIDTH  = $clog2( MUX_SIZE )
  )
  ( input  wire                  clk
  , input  wire                  signal_load
  , input  wire            [1:0] signal_mode
  , input  wire                  signal_sel
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out
  );


reg [DATA_WIDTH-1:0] data;
reg [ATTR_WIDTH-1:0] attr;
always @(posedge clk) { attr, data } <= { attr_in, data_in };

reg load;
reg sel;
reg [1:0] mode;
always @(posedge clk) load <= signal_load;
always @(posedge clk) sel <= signal_sel && signal_load;
always @(posedge clk) mode <= signal_mode;

reg [SEL_WIDTH-1:0] port_SEL;
always @(posedge clk)
  if ( sel )       port_SEL <= data[ DATA_WIDTH-1 : DATA_WIDTH - SEL_WIDTH ] >> (3 - mode);

reg [SEL_WIDTH-1:0] port_N;
always @(posedge clk)
  if       ( sel ) port_N <= 0;
  else if ( load ) port_N <= port_N + 1;


reg [DATA_WIDTH-1:0] data_latch;
reg [ATTR_WIDTH-1:0] attr_latch;
always @(posedge clk)
  if ( port_SEL == port_N ) { attr_latch, data_latch } <= { attr, data };


always @(posedge clk)
  if ( ~signal_oe ) { attr_out, data_out } <= 0;
  else              { attr_out, data_out } <= { attr_latch, data_latch };

endmodule
