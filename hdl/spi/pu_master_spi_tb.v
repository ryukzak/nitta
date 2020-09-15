`timescale 1 ms/ 1 ms

module pu_master_spi_tb
  #( parameter DATA_WIDTH      = 32
   , parameter ATTR_WIDTH      = 4
   , parameter SPI_DATA_WIDTH  = 8
   , parameter BUF_SIZE        = 8
   )
  ();

reg clk;
reg rst;
reg start_transaction;

reg  [DATA_WIDTH-1:0] data_in;
wire [DATA_WIDTH-1:0] data_out;

wire mosi, sclk, cs;
reg miso;

reg wr;
reg oe;
reg signal_cycle;

pu_master_spi #
  ( .DATA_WIDTH( DATA_WIDTH )
  ) net
  ( .clk( clk )
  , .rst( rst )
  , .signal_wr( wr )
  , .data_in( data_in )
  , .start_transaction( start_transaction )
  , .signal_cycle( signal_cycle )
  , .miso( miso )
  , .mosi( mosi )
  );

always begin
  #5 clk <= ~clk;
end

initial begin
  clk <= 0; rst <= 1; @(posedge clk);
  rst <= 0;
  wr  <= 0;
  data_in <= 0;
  start_transaction <= 0;
  signal_cycle <= 0;
  $display("Start");
end

initial begin
  @(negedge rst);

  data_in <= 32'hA1B1C1D1; wr <= 1; @(posedge clk);
                           wr <= 0; @(posedge clk);
  data_in <= 32'hA2B2C2D2; wr <= 1; @(posedge clk);
                           wr <= 0; @(posedge clk);

  start_transaction <= 1;           @(posedge clk);
  start_transaction <= 0;

  repeat(300) @(posedge clk);

  signal_cycle <= 1;                @(posedge clk);
  signal_cycle <= 0;                @(posedge clk);

  data_in <= 32'hA3B3C3D3; wr <= 1; @(posedge clk);
                           wr <= 0; @(posedge clk);
  data_in <= 32'hA4B4C4D4; wr <= 1; @(posedge clk);
                           wr <= 0; @(posedge clk);

  start_transaction <= 1;           @(posedge clk);
  start_transaction <= 0;

  repeat(300) @(posedge clk);

  signal_cycle <= 1;                @(posedge clk);
  signal_cycle <= 0;                @(posedge clk);

  data_in <= 32'hA5B5C5D5; wr <= 1; @(posedge clk);
                           wr <= 0; @(posedge clk);
  data_in <= 32'hA6B6C6D6; wr <= 1; @(posedge clk);
                           wr <= 0; @(posedge clk);

  start_transaction <= 1;           @(posedge clk);
  start_transaction <= 0;

  repeat(300) @(posedge clk);

  repeat(10) @(posedge clk); $finish;
end

initial begin
  $dumpfile("pu_master_spi_tb.vcd");
  $dumpvars(0, pu_master_spi_tb);
end

endmodule
