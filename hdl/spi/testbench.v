`timescale 1 ms/ 1 ms
module pu_slave_spi_tb
();

reg clk;
reg rst;
reg wr;
reg oe;

spi_buffer spi_buffer_test(
  .clk( clk )
, .rst( rst )
, .wr ( wr )
, .oe( oe )
);

always begin
  #5 clk = ~clk;
end

initial begin
  clk = 0; wr = 0;             @(posedge clk);
  rst = 1;                     @(posedge clk);
  rst = 0;                     @(posedge clk);
  $display("Start");
end

initial begin
  $dumpfile("pu_slave_spi_tb.vcd");
  $dumpvars(0, pu_slave_spi_tb);
  $display("finish");
end

initial begin 
  @(negedge rst);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);
  repeat(8) @(posedge clk); wr = 1; @(posedge clk); wr = 0; @(posedge clk);

  
  
  repeat(108) @(posedge clk);



  $finish;
end



endmodule