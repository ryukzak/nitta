`timescale 1 ps/ 1 ps

module pu_div_tb
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter INVALID    = 0
   , parameter count_clk  = 32
   );

reg                        rst;
reg                        clk;
reg                 signal_sel;
reg                  signal_wr;
reg  [DATA_WIDTH-1:0]  data_in;
reg  [ATTR_WIDTH-1:0]  attr_in;
reg                  signal_oe;
wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;


pu_div
  #( .DATA_WIDTH( DATA_WIDTH )
   , .ATTR_WIDTH( ATTR_WIDTH )
   , .INVALID( INVALID )
   ) i1
// port map - connection between master ports and signals/registers
  ( .rst(rst)
  , .clk(clk)

  , .signal_sel(signal_sel)

  , .signal_wr(signal_wr)
  , .data_in(data_in)
  , .attr_in(attr_in)

  , .signal_oe(signal_oe)
  , .data_out(data_out)
  , .attr_out(attr_out)
  );

initial begin
  clk = 0;
  forever #10 clk = !clk;
end

initial begin
  $display("Start programm");

  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0;
  rst <= 1; repeat (2) @(posedge clk);
  rst <= 0; @(posedge clk);

  // -102 / 4 = -25 + -2 (loading)
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= -102; attr_in <= 0;  @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= 4;    attr_in <= 0;  @(posedge clk);

  // -102 / 4 = -25 + -2 (execute)
  signal_oe <= 1; signal_wr <= 1; signal_sel <= 1; data_in <= 4;    attr_in <= 0;  @(posedge clk);

  repeat(4)@(posedge clk);

// -102 / 4 = -25 + -2 (result)
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 1; data_in <= 0; attr_in <= 0; @(posedge clk);

  // with pipeline

  // 4 / 2 = 2 + 0 (loading)
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= 4; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= 2; attr_in <= 0; @(posedge clk);

  // 4 / 2 = 2 + 0 (execute)
  signal_oe <= 1; signal_wr <= 1; signal_sel <= 1; data_in <= 4; attr_in <= 0; @(posedge clk);

  // 9 / 5 = 1 + 4 (loading)
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= 5; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= 9; attr_in <= 0; @(posedge clk);

  // 9 / 5 = 1 + 4 (execute)
  signal_oe <= 1; signal_wr <= 1; signal_sel <= 1; data_in <= 4; attr_in <= 0; @(posedge clk);

  @(posedge clk);

  // 4 / 2 = 2 + 0 (result)
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 1; data_in <= 0; attr_in <= 0; @(posedge clk);

  @(posedge clk);

  // 9 / 5 = 1 + 4 (result)
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 1; data_in <= 0; attr_in <= 0; @(posedge clk);

  repeat (20) @(posedge clk);
  $finish();
end

initial begin
  $dumpfile("pu_div_tb.vcd");
  $dumpvars(0, pu_div_tb);
end

endmodule
