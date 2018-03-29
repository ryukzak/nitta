`timescale 1 ms/ 1 ms
module pu_slave_spi_tb
  #( parameter DATA_WIDTH      = 32
   , parameter ATTR_WIDTH      = 4
  ,  parameter SPI_DATA_WIDTH  = 8
  ,  parameter SCLK_HALFPERIOD = 1
  ,  parameter BUF_SIZE        = 10
  )
  ();

reg clk;
reg rst;
reg start_transaction;
reg  [DATA_WIDTH-1:0] master_in;
wire [DATA_WIDTH-1:0] master_out;
reg  [SPI_DATA_WIDTH-1:0] slave_in; 

reg  [DATA_WIDTH-1:0] pu_spi_data_in;

wire mosi, miso, sclk, cs;

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;
reg wr;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;
reg oe;
reg cycle;

spi_master_driver #( .DATA_WIDTH( DATA_WIDTH ) ) master 
  ( .clk(clk)
  , .rst(rst)
  , .start_transaction(start_transaction)
  , .data_in(master_in)
  , .data_out(master_out)
  , .ready(ready)
  , .miso( miso )
  , .mosi( mosi )
  , .sclk( sclk )
  , .cs( cs )
  );

spi_slave_driver #( .DATA_WIDTH( SPI_DATA_WIDTH ) ) slave
  ( .clk(clk)
  , .rst(rst)
  , .data_in(slave_in)
  , .miso( miso )
  , .mosi( mosi )
  , .ready(ready)
  , .sclk( sclk )
  , .cs( cs )
  );

always begin
  #5 clk = ~clk;
end

initial begin
  clk = 0;                     @(posedge clk);
  rst = 1;                     @(posedge clk);
  rst = 0;                     @(posedge clk);
  $display("Start");
end

initial begin
  $dumpfile("pu_slave_spi_tb.vcd");
  $dumpvars(0, pu_slave_spi_tb);
  $display("finish");
end


initial begin // spi communication
  @(negedge rst);

  master_in = 32'hA1A2A3A4;   slave_in = 8'hFF;          @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);
  repeat(108) @(posedge clk);
  // slave_in = 8'h22;      repeat(12) @(posedge clk);
  // slave_in = 8'h33;      repeat(12) @(posedge clk);
  // slave_in = 8'h44;     repeat(12) @(posedge clk);

  // repeat(108) @(posedge clk);

  // master_in = 32'hB1B2B3B4;                              @(posedge clk);
  // start_transaction = 1;                                 @(posedge clk);
  // start_transaction = 0;                                 @(posedge clk);

  // repeat(108) @(posedge clk);

  // master_in = 32'hC1C2C3C4;                              @(posedge clk);
  // start_transaction = 1;                                 @(posedge clk);
  // start_transaction = 0;                                 @(posedge clk);

  // repeat(108) @(posedge clk);

  // master_in = 32'hD1D2D3D4;                              @(posedge clk);
  // start_transaction = 1;                                 @(posedge clk);
  // start_transaction = 0;                                 @(posedge clk);

  // repeat(108) @(posedge clk);

  // master_in = 32'hA1A2A3A4;                              @(posedge clk);
  // start_transaction = 1;                                 @(posedge clk);
  // start_transaction = 0;                                 @(posedge clk);

  // repeat(108) @(posedge clk);

  // master_in = 32'hB1B2B3B4;                              @(posedge clk);
  // start_transaction = 1;                                 @(posedge clk);
  // start_transaction = 0;                                 @(posedge clk);

  // repeat(108) @(posedge clk);

  // master_in = 32'hC1C2C3C4;                              @(posedge clk);
  // start_transaction = 1;                                 @(posedge clk);
  // start_transaction = 0;                                 @(posedge clk);

  // repeat(108) @(posedge clk);

  // master_in = 32'hD1D2D3D4;                              @(posedge clk);
  // start_transaction = 1;                                 @(posedge clk);
  // start_transaction = 0;                                 @(posedge clk);


  $finish;
end



endmodule