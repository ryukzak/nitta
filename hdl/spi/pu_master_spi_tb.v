`timescale 1 ms/ 1 ms
module pu_master_spi_tb
  #( parameter DATA_WIDTH      = 32
   , parameter ATTR_WIDTH      = 4
   , parameter SPI_DATA_WIDTH  = 8
   , parameter BUF_SIZE        = 8
   )
  ();

reg clk; 

pu_master_spi#
    ( .DATA_WIDTH( 32 )
    ) net
    ( .clk( clk )
    );

always begin
  clk <= 1;
  repeat(4) #5 clk <= ~clk;
  forever #5 clk <= ~clk;
end 

initial begin
  clk <= 0; @(posedge clk);
  $display("Start");
end

initial begin
  $dumpfile("pu_master_spi_tb.vcd");
  $dumpvars(0, pu_master_spi_tb);
end

initial begin 
  $finish;
end

endmodule