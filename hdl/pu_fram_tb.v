module pu_fram_tb();

parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;

reg clk, rst;
reg wr, oe;
reg [3:0] addr;

reg [DATA_WIDTH-1:0]  data_in;
reg [ATTR_WIDTH-1:0]  attr_in;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;

pu_fram
  #( .RAM_SIZE( 16 )
   , .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 4 )
  ) u
  ( .clk( clk )

  , .signal_addr( addr )
  , .signal_wr( wr )
  , .data_in(data_in)
  , .attr_in(attr_in)

  , .signal_oe(oe)
  , .data_out(data_out)
  , .attr_out(attr_out)
  );

initial begin
  $dumpfile("pu_fram_tb.vcd");
  $dumpvars(0, pu_fram_tb);
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

    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 1; oe <= 0; data_in <= 'hAAAAAAAA; attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 0; oe <= 1; data_in <= 0;          attr_in <= 0;    @(posedge clk);

    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);

    addr <= 3; wr <= 1; oe <= 0; data_in <= 'hBBBBBBBB; attr_in <= 0;    @(posedge clk);
    addr <= 4; wr <= 1; oe <= 0; data_in <= 'hCCCCCCCC; attr_in <= 0;    @(posedge clk);
    addr <= 5; wr <= 1; oe <= 0; data_in <= 'hDDDDDDDD; attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 4; wr <= 0; oe <= 1; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 5; wr <= 0; oe <= 1; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 3; wr <= 0; oe <= 1; data_in <= 0;          attr_in <= 0;    @(posedge clk);

    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);
    addr <= 0; wr <= 0; oe <= 0; data_in <= 0;          attr_in <= 0;    @(posedge clk);

    $finish;
  end

endmodule
