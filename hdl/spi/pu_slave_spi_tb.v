`timescale 1 ms/ 1 ms
module pu_slave_spi_tb
  #( parameter DATA_WIDTH      = 32
   , parameter ATTR_WIDTH      = 4
   , parameter SPI_DATA_WIDTH  = 8
   , parameter BUF_SIZE        = 8
   )
  ();

reg clk, slow_clk;
reg rst;
reg start_transaction;
reg  [DATA_WIDTH*2-1:0] master_in;
wire [DATA_WIDTH*2-1:0] master_out;
wire ready;

reg  [DATA_WIDTH-1:0] pu_spi_data_in;

wire mosi, miso, sclk, cs;

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;
reg wr;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;
reg oe;
reg cycle;

spi_master_driver
  #( .DATA_WIDTH( 64 )
   , .SCLK_HALFPERIOD( 1 )
   ) master
  ( .clk(slow_clk)
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
initial master.inner.shiftreg <= 0; // для ясности

pu_slave_spi #( .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
              , .BUF_SIZE( BUF_SIZE )
              , .BOUNCE_FILTER( 1 )
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

  //, .flag_stop( ready )

  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );

always begin
  #5  clk <= ~clk;
end
always begin
  #20 slow_clk <= ~slow_clk;
end


initial begin
  clk <= 0; slow_clk <= 0; cycle <= 0; start_transaction <= 0; @(posedge clk);
  rst <= 1;                                   @(posedge clk);
  rst <= 0;                                   @(posedge clk);
  $display("Start");
end

initial begin
  pu.n2s_buffer1.memory[0] = 32'hE0E1E2E3;
  pu.n2s_buffer1.memory[1] = 32'hE4E5E6E7;
  pu.n2s_buffer1.memory[2] = 32'hE8E9EAEB;
  pu.n2s_buffer1.memory[3] = 32'hECEDEEEF;

  pu.n2s_buffer1.memory[0] = 32'hF0F1F2F3;
  pu.n2s_buffer1.memory[1] = 32'hF4F5F6F7;
  pu.n2s_buffer1.memory[2] = 32'hF8F9FAFB;
  pu.n2s_buffer1.memory[3] = 32'hFCFDFEFF;

  $dumpfile("pu_slave_spi_tb.vcd");
  $dumpvars(0, pu_slave_spi_tb);
  $display("finish");
end

// Computing cycle, including:
// 1. Reading data received via SPI. We get 1 word.
// 2. Writing data to SPI. We write two words.

task display_buffers;
  integer i;
  begin
    for ( i = 0; i < BUF_SIZE; i = i + 1 ) begin
      $display("%d -> %h %h", i, pu.n2s_buffer0.memory[i], pu.n2s_buffer1.memory[i]);
    end
  end
endtask


initial begin
  pu_spi_data_in <= 32'h00000000; oe <= 0; wr <= 0;
  @(negedge rst);

  /////////////////// First cycle.
  cycle <= 1;                              @(posedge clk);
  cycle <= 0;                  repeat(199) @(posedge clk);
  oe <= 1;                                 @(posedge clk); // We are waiting for the invalid flag on the bus, since there were no new downloads after RST.
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hB0B1B2B3; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hB4B5B6B7; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);
  repeat(600) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  display_buffers();

  /////////////////// Second cycle.
  cycle <= 1;                              @(posedge clk); // Second cycle start signal
  cycle <= 0;                  repeat( 99) @(posedge clk);
  oe <= 1;                                 @(posedge clk);  // Waiting in the buffer: A0A1A2A3A4A5A6A7
                                                            // on wire A0A1A2A3
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hB8B9BABB; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hBCBDBEBF; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);
  repeat(600) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  display_buffers();

  /////////////////// Third cycle.
  cycle <= 1;                              @(posedge clk);
  cycle <= 0;                  repeat( 99) @(posedge clk);
  oe <= 1;                                 @(posedge clk); // We are waiting for the invalid flag on the bus, since there were no new downloads after RST.
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hC0C1C2C3; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hC4C5C6C7; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);
  repeat(600) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  display_buffers();


  /////////////////// Fourth cycle.
  cycle <= 1;                              @(posedge clk); // Second cycle start signal
  cycle <= 0;                  repeat( 99) @(posedge clk);
  oe <= 1;                                 @(posedge clk);  // Waiting in the buffer: A0A1A2A3A4A5A6A7
                                                            // On wire A0A1A2A3
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hC8C9CACB; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hCCCDCECF; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  display_buffers();

end


initial begin // spi communication
  @(negedge rst);

  repeat(35) @(posedge slow_clk);

// Two words should appear in the input buffer: A1A2A3A4 and A5A6A7A8.
// Two words from the output buffer must go to the pipe.
  master_in = 64'hA0A1A2A3A4A5A6A7;                        @(posedge slow_clk);
  start_transaction = 1;                                   @(posedge slow_clk);
  start_transaction = 0;                                   @(posedge slow_clk);
  // In this case, we expect to receive a default value from the channel hCCCCCCCC,
  // since the buffer was not full for sending.
  repeat(130) @(posedge slow_clk);
  repeat(35) @(posedge slow_clk);



  repeat(35) @(posedge slow_clk);

  // Two words should appear in the input buffer: A9AAABAC and ADAEAFA0.
  // Two words from the output buffer should go to the channel.
  master_in = 64'hA8A9AAABACADAEAF;                        @(posedge slow_clk);
  start_transaction = 1;                                   @(posedge slow_clk);
  start_transaction = 0;                                   @(posedge slow_clk);
  // From the channel, we expect to get the value hB0B1B2B3B4B5B6B7
  repeat(130 - 3) @(posedge slow_clk);
  repeat(35) @(posedge slow_clk);



  repeat(35) @(posedge slow_clk);
  // Two words should appear in the input buffer: A9AAABAC and ADAEAFA0.
  // Two words from the output buffer should go to the channel.
  master_in = 64'hA8A9AAABACADAEAF;                        @(posedge slow_clk);
  start_transaction = 1;                                   @(posedge slow_clk);
  start_transaction = 0;                                   @(posedge slow_clk);
  // From the channel, we expect to get the value hB0B1B2B3B4B5B6B7
  repeat(130 - 3) @(posedge slow_clk);
  repeat(35) @(posedge slow_clk);



  repeat(35) @(posedge slow_clk);
  // Two words should appear in the input buffer: A9AAABAC and ADAEAFA0.
  // Two words should go to the channel from the output buffer.

  master_in = 64'hA8A9AAABACADAEAF;                        @(posedge slow_clk);
  start_transaction = 1;                                   @(posedge slow_clk);
  start_transaction = 0;                                   @(posedge slow_clk);
  // From the channel, we expect to get the value hB0B1B2B3B4B5B6B7
  repeat(130 - 3) @(posedge slow_clk);
  repeat(35) @(posedge slow_clk);



  repeat(60) @(posedge slow_clk);

  $finish;
end

endmodule
