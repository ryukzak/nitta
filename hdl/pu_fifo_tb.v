module pu_fifo_tb ();

parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;
parameter FIFO_SIZE = 3;

reg clk, rst, signal_wr, signal_oe;

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;

pu_fifo
#( .DATA_WIDTH( 32 )
,  .ATTR_WIDTH( 4 )
,  .FIFO_SIZE( 3 )
) u (
.clk(clk),
.rst(rst),
.data_in(data_in),
.attr_in(attr_in),
.signal_wr(signal_wr),

.signal_oe(signal_oe),
.data_out(data_out),
.attr_out(attr_out)
);

initial
  begin
    rst <= 1; @(posedge clk); rst <= 0;
  end

initial 
    begin
      $dumpfile("pu_fifo_tb.vcd");
      $dumpvars(0, pu_fifo_tb);
      
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 0; @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 0; @(posedge clk);

      data_in <= 11; signal_wr <= 1;  attr_in <= 3;  signal_oe <= 0;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 1;  @(posedge clk);
      data_in <= 12; signal_wr <= 1;  attr_in <= 4;  signal_oe <= 0;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 0;  @(posedge clk);
      data_in <= 13; signal_wr <= 1;  attr_in <= 5;  signal_oe <= 0;  @(posedge clk);

      data_in <= 14; signal_wr <= 1;  attr_in <= 6;  signal_oe <= 0;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 1;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 0;  @(posedge clk); 
      data_in <= 15; signal_wr <= 1;  attr_in <= 7;  signal_oe <= 0;  @(posedge clk);    

      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 1;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 0;  @(posedge clk);   
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 1;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 0;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 1;  @(posedge clk);
      data_in <= 0;  signal_wr <= 0;  attr_in <= 0;  signal_oe <= 0;  @(posedge clk);             
      $finish;
    end

initial
  begin
    clk = 0;
    forever #1 clk = !clk;
  end

endmodule