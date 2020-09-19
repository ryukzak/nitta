`timescale 1 ps/ 1 ps

module pu_div_tb
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter INVALID    = 0
   , parameter count_clk  = 32
   );

reg                        rst;
reg                        clk;
reg  [DATA_WIDTH-1:0]  data_in;
reg  [ATTR_WIDTH-1:0]  attr_in;
reg                  signal_wr;
reg              signal_wr_sel;
wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;
reg                  signal_oe;
reg              signal_oe_sel;


pu_div
  #( .DATA_WIDTH( DATA_WIDTH )
   , .ATTR_WIDTH( ATTR_WIDTH )
   , .INVALID( INVALID )
   ) i1
// port map - connection between master ports and signals/registers
  ( .data_in(data_in)
  , .signal_oe(signal_oe)
  , .rst(rst)
  , .signal_wr(signal_wr)
  , .clk(clk)
  , .signal_wr_sel(signal_wr_sel)
  , .signal_oe_sel(signal_oe_sel)
  , .data_out(data_out)
  , .attr_in(attr_in)
  , .attr_out(attr_out)
  );

initial begin
  clk = 0;
  forever #10 clk = !clk;
end

initial begin
  $display("Start programm");

  signal_oe <= 0; signal_wr <= 0; signal_wr_sel <= 0; signal_oe_sel <= 0; data_in <= 0; attr_in <= 0;
  rst <= 1; repeat (2) @(posedge clk);
  rst <= 0; @(posedge clk);

// -100 / 4 = -25 (loading)
  signal_oe <= 0; signal_wr <= 1; signal_wr_sel <= 0; signal_oe_sel <= 0; data_in <= -100; attr_in <= 1;  @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_wr_sel <= 1; signal_oe_sel <= 0; data_in <= 4;    attr_in <= 1;  @(posedge clk);

// 100 / -5 = -20 (loading)
  signal_oe <= 0; signal_wr <= 1; signal_wr_sel <= 0; signal_oe_sel <= 0; data_in <= 100; attr_in <= 0;  @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_wr_sel <= 1; signal_oe_sel <= 0; data_in <= -5;  attr_in <= 0;  @(posedge clk);

// -100 / 4 = -25 (result)
  signal_oe <= 0; signal_wr <= 0; signal_wr_sel <= 0; signal_oe_sel <= 0; data_in <= 0; attr_in <= 0; repeat (2) @(posedge clk);
  signal_oe <= 1; signal_wr <= 0; signal_wr_sel <= 0; signal_oe_sel <= 1; data_in <= 0; attr_in <= 0; repeat (2) @(posedge clk);

// 100 / -5 = -20 (result), -100 / -5 = 25 (loading)
  signal_oe <= 1; signal_wr <= 1; signal_wr_sel <= 0; signal_oe_sel <= 1; data_in <= -100; attr_in <= 1;  @(posedge clk);
  signal_oe <= 1; signal_wr <= 1; signal_wr_sel <= 1; signal_oe_sel <= 1; data_in <= -5;   attr_in <= 0;  @(posedge clk);

// 100 / 0 = x (loading)
  signal_oe <= 1; signal_wr <= 1; signal_wr_sel <= 0; signal_oe_sel <= 1; data_in <= 100; attr_in <= 0;  @(posedge clk);
  signal_oe <= 1; signal_wr <= 1; signal_wr_sel <= 1; signal_oe_sel <= 1; data_in <= 0;   attr_in <= 1;  @(posedge clk);

// 100 / 0 = x (result)
  signal_oe <= 1; signal_wr <= 0; signal_wr_sel <= 0; signal_oe_sel <= 1; data_in <= 0; attr_in <= 0; repeat (50) @(posedge clk);

// -100 / 4 = -25 (loading)
  signal_oe <= 0; signal_wr <= 1; signal_wr_sel <= 0; signal_oe_sel <= 0; data_in <= -100; attr_in <= 1;  @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_wr_sel <= 1; signal_oe_sel <= 0; data_in <= 4;    attr_in <= 1;  @(posedge clk);

// -100 / 4 = -25 (result)
  signal_oe <= 1; signal_wr <= 0; signal_wr_sel <= 0; signal_oe_sel <= 1; data_in <= 0;    attr_in <= 0; repeat (20) @(posedge clk);

  repeat (20) @(posedge clk);
  $finish();
end

initial begin
  $dumpfile("pu_div_tb.vcd");
  $dumpvars(0, pu_div_tb);
end

endmodule
