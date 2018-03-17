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
reg  [SPI_DATA_WIDTH-1:0] master_in;
wire [SPI_DATA_WIDTH-1:0] master_out;
reg  [SPI_DATA_WIDTH-1:0] slave_in;
wire [SPI_DATA_WIDTH-1:0] slave_out;

reg [DATA_WIDTH-1:0] pu_spi_data_in;

wire mosi, miso, sclk, cs;

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;
reg wr;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;
reg oe;

spi_master_driver master 
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

pu_slave_spi #( .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
              , .BUF_SIZE( BUF_SIZE )
              ) pu 
  ( .clk( clk )
  , .rst( rst )
  // , .signal_cycle( cycle )

  , .signal_wr( wr )
  , .data_in( pu_spi_data_in )
  , .attr_in( attr_in )

  , .signal_oe( oe )
  , .data_out( data_out )
  , .attr_out( attr_out )

  // , .flag_start( start )
  , .flag_stop( ready )

  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );

integer i;

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

initial begin
  @(negedge rst);



  pu_spi_data_in = 32'h99888766; @(posedge clk);
  wr <= 1;                       @(posedge clk);
  wr <= 0;                       @(posedge clk);

  pu_spi_data_in = 32'h67564466; @(posedge clk);
  wr <= 1;                       @(posedge clk);
  wr <= 0;                       @(posedge clk);

  pu_spi_data_in = 32'h75867857; @(posedge clk);
  wr <= 1;                       @(posedge clk);
  wr <= 0;                       @(posedge clk);

  pu_spi_data_in = 32'h54345634; @(posedge clk);
  wr <= 1;                       @(posedge clk);
  wr <= 0;                       @(posedge clk);

  oe <= 1;                       @(posedge clk);
  oe <= 0;                       @(posedge clk);

  oe <= 1;                       @(posedge clk);
  oe <= 0;                       @(posedge clk);

  oe <= 1;                       @(posedge clk);
  oe <= 0;                       @(posedge clk);

  oe <= 1;                       @(posedge clk);
  oe <= 0;                       @(posedge clk);

  repeat(10) @(posedge clk);

  master_in = 8'h11;                                 @(posedge clk);
  start_transaction = 1;                             @(posedge clk);
  start_transaction = 0;                             @(posedge clk);

  repeat(18) @(posedge clk);

  master_in = 8'h22;                                 @(posedge clk);
  start_transaction = 1;                             @(posedge clk);
  start_transaction = 0;                             @(posedge clk);

  repeat(18) @(posedge clk);

  master_in = 8'h33;
  start_transaction = 1;                             @(posedge clk);
  start_transaction = 0;                             @(posedge clk);

  repeat(18) @(posedge clk);

  master_in = 8'h44;
  start_transaction = 1;                             @(posedge clk);
  start_transaction = 0;                             @(posedge clk);


  repeat(35) @(posedge clk); 

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  for ( i = 0; i < BUF_SIZE; i = i + 1 )
    begin
      $display("%d -> %h , %h , %h, %h", i, pu.receive_buffer.memory[i], pu.transfer_buffer.memory[i], pu.transfer_buffer.memory_nitta[i], pu.send_buffer.memory[i]);
    end

  $finish;
end

// initial begin
//   wr <= 0; data_in <= 0; attr_in <= 0;
//   @(negedge rst);
//   data_in <= 'h42; attr_in <= 0; wr <= 1; @(posedge clk);
//   data_in <= 'h0; attr_in <= 0; wr <= 0; @(posedge clk);
//   data_in <= 'h0; attr_in <= 0; wr <= 0; @(posedge clk);
//   data_in <= 'h43; attr_in <= 0; wr <= 1; @(posedge clk);
//   data_in <= 'h0; attr_in <= 0; wr <= 1; @(posedge clk);
//   data_in <= 'h0; attr_in <= 0; wr <= 0; @(posedge clk);
//   data_in <= 'h37; attr_in <= 0; wr <= 1; @(posedge clk);
//   data_in <= 'h0; attr_in <= 0; wr <= 0; @(posedge clk);
//   repeat(18) @(posedge clk);
// end

// initial begin
//   oe <= 0;
//   @(negedge rst);
//   oe <= 0; @(posedge clk);
//   oe <= 0; @(posedge clk);
//   oe <= 1; @(posedge clk);
//   oe <= 0; @(posedge clk);
//   oe <= 1; @(posedge clk);
//   oe <= 1; @(posedge clk);
//   oe <= 0; @(posedge clk);
//   oe <= 0; @(posedge clk);
//   repeat(18) @(posedge clk);
// end


endmodule