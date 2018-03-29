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

reg  [DATA_WIDTH-1:0] pu_spi_data_in;

wire mosi, miso, sclk, cs;

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;
reg wr;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;
reg oe;
reg cycle;

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

// spi_slave_driver slave
//   ( .clk(clk)
//   , .rst(rst)
//   , .data_in(slave_in)
//   , .miso( miso )
//   , .mosi( mosi )
//   , .ready(ready)
//   , .sclk( sclk )
//   , .cs( cs )
//   );

pu_slave_spi #( .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
              , .BUF_SIZE( BUF_SIZE )
              ) pu
  ( .clk( clk )
  , .rst( rst )
  , .signal_cycle( cycle )

  , .signal_wr( wr )
  , .data_in( pu_spi_data_in )
  , .attr_in( attr_in )

  , .signal_oe( oe )
  , .data_out( data_out )
  , .attr_out( attr_out )

  // , .flag_start( start )
  //, .flag_stop( ready )

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
  oe <= 0;
  cycle <= 0;   repeat(200) @(posedge clk);
  cycle <= 1;               @(posedge clk);

  cycle <= 0;   repeat(130) @(posedge clk);

  oe <= 1;                  @(posedge clk);
  oe <= 0;                  @(posedge clk);

  oe <= 1;                  @(posedge clk);
  oe <= 0;                  @(posedge clk);

  cycle <= 0;   repeat(68) @(posedge clk);

  cycle <= 1;               @(posedge clk);

  cycle <= 0;   repeat(130) @(posedge clk);

  oe <= 1;                  @(posedge clk);
  oe <= 0;                  @(posedge clk);

  cycle <= 0;   repeat(68) @(posedge clk);

  cycle <= 1;               @(posedge clk);

  cycle <= 0;   repeat(200) @(posedge clk);
  cycle <= 1;               @(posedge clk);

  cycle <= 0;   repeat(200) @(posedge clk);
  cycle <= 1;               @(posedge clk);
end

// initial begin // nitta communication
//   @(negedge rst);


  // pu_spi_data_in <= 32'hA1A2A3A4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // pu_spi_data_in <= 32'hB1B2B3B4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // pu_spi_data_in <= 32'hC1C2C3C4; wr <= 1; oe <= 0;     @(posedge clk);

  // pu_spi_data_in <= 32'hD1D2D3D4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);


  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // repeat(100) @(posedge clk);

  // pu_spi_data_in <= 32'hA2A2A3A4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // repeat(100) @(posedge clk);

  // pu_spi_data_in <= 32'hB2B2B3B4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // pu_spi_data_in <= 32'hC2C2C3C4; wr <= 1; oe <= 0;     @(posedge clk);

  // pu_spi_data_in <= 32'hD2D2D3D4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // repeat(500) @(posedge clk);

  // pu_spi_data_in <= 32'hB3B2B3B4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // pu_spi_data_in <= 32'hC3C2C3C4; wr <= 1; oe <= 0;     @(posedge clk);

  // pu_spi_data_in <= 32'hD3D2D3D4; wr <= 1; oe <= 0;     @(posedge clk);
  // pu_spi_data_in <= 32'h00000000; wr <= 0; oe <= 0;     @(posedge clk);

  // repeat(100)          @(posedge clk);
  // oe <= 1;   repeat(5) @(posedge clk);
  // oe <= 0;             @(posedge clk);
  // repeat(10) @(posedge clk);


  // $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  // for ( i = 0; i < BUF_SIZE; i = i + 1 )
  //   begin
  //     $display("%d -> %h , %h , %h, %h", i, pu.receive_buffer.memory[i], pu.transfer_out_buffer.memory[i], pu.transfer_in_buffer.memory[i], pu.send_buffer.memory[i]);
  //   end

//end

initial begin // spi communication
  @(negedge rst);

  master_in = 32'hA1A2A3A4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);

  repeat(108) @(posedge clk);

  master_in = 32'hB1B2B3B4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);

  repeat(108) @(posedge clk);

  master_in = 32'hC1C2C3C4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);

  repeat(108) @(posedge clk);

  master_in = 32'hD1D2D3D4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);

  repeat(108) @(posedge clk);

  master_in = 32'hA1A2A3A4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);

  repeat(108) @(posedge clk);

  master_in = 32'hB1B2B3B4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);

  repeat(108) @(posedge clk);

  master_in = 32'hC1C2C3C4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);

  repeat(108) @(posedge clk);

  master_in = 32'hD1D2D3D4;                              @(posedge clk);
  start_transaction = 1;                                 @(posedge clk);
  start_transaction = 0;                                 @(posedge clk);


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