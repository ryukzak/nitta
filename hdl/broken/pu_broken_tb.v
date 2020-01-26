module broken_pu_tb();

parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;

reg clk, rst;
reg wr, oe;

reg [DATA_WIDTH-1:0]  data_in;
reg [ATTR_WIDTH-1:0]  attr_in;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;

broken_pu
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 4 )
   , .IS_BROKEN( 0 )
  ) u
  ( .clk( clk )

  , .signal_wr( wr )
  , .data_in(data_in)
  , .attr_in(attr_in)

  , .signal_oe(oe)
  , .data_out(data_out)
  , .attr_out(attr_out)
  );

initial begin
  $dumpfile("broken_pu_tb.vcd");
  $dumpvars(0, broken_pu_tb);
  clk <= 1;
  forever #1 clk <= !clk;
end

initial begin
  rst <= 1;     @(posedge clk);
  rst <= 0;
end

initial
  begin
    @(negedge rst);

    wr <= 0; oe <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
    wr <= 0; oe <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
    wr <= 1; oe <= 0; data_in <= 2; attr_in <= 2; @(posedge clk);
    wr <= 0; oe <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
    wr <= 0; oe <= 1; data_in <= 0; attr_in <= 0; @(posedge clk);
    wr <= 0; oe <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
    wr <= 0; oe <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);
    wr <= 0; oe <= 0; data_in <= 0; attr_in <= 0; @(posedge clk);

    $finish;
  end

endmodule
