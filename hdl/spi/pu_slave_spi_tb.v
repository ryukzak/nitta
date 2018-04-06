`timescale 1 ms/ 1 ms
module pu_slave_spi_tb
  #( parameter DATA_WIDTH      = 32
   , parameter ATTR_WIDTH      = 4
   , parameter SPI_DATA_WIDTH  = 8
   , parameter BUF_SIZE        = 8
   )
  ();

reg clk;
reg rst;
reg start_transaction;
reg  [DATA_WIDTH*2-1:0] master_in;
wire [DATA_WIDTH*2-1:0] master_out;

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
initial master.inner.shiftreg <= 0; // для ясности

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
  clk = 0; cycle = 0; start_transaction = 0; @(posedge clk);
  rst = 1;                                   @(posedge clk);
  rst = 0;                                   @(posedge clk);
  $display("Start");
end

initial begin
  $dumpfile("pu_slave_spi_tb.vcd");
  $dumpvars(0, pu_slave_spi_tb);
  $display("finish");
end

// Вычислительный цикл, включая:
// 1. Чтение полученных через SPI данных. Получаем 1 слово.
// 2. Запись данных в SPI. Пишем два слова.

initial begin 
  pu_spi_data_in <= 32'h00000000; oe <= 0; wr <= 0;
  @(negedge rst);

  /////////////////// Первый цикл.
  cycle <= 1;                              @(posedge clk); 
  cycle <= 0;                  repeat( 99) @(posedge clk);
  oe <= 1;                                 @(posedge clk); // Ожидаем на шине флаг invalid, так как после RST новых загрузок небыло.  
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hB0B1B2B3; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hB4B5B6B7; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  for ( i = 0; i < BUF_SIZE; i = i + 1 )
    begin
      $display("%d -> %h , %h , %h, %h", i, pu.receive_buffer.memory[i], pu.transfer_out_buffer.memory[i], pu.transfer_in_buffer.memory[i], pu.send_buffer.memory[i]);
    end

  /////////////////// Второй цикл.
  cycle <= 1;                              @(posedge clk); // сигнал о начале второго цикла
  cycle <= 0;                  repeat( 99) @(posedge clk);
  oe <= 1;                                 @(posedge clk);  // В буфере ожидаем: A0A1A2A3A4A5A6A7
                                                            // На шине A0A1A2A3
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hB8B9BABB; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hBCBDBEBF; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  for ( i = 0; i < BUF_SIZE; i = i + 1 )
    begin
      $display("%d -> %h , %h , %h, %h", i, pu.receive_buffer.memory[i], pu.transfer_out_buffer.memory[i], pu.transfer_in_buffer.memory[i], pu.send_buffer.memory[i]);
    end

  /////////////////// Третий цикл.
  cycle <= 1;                              @(posedge clk); 
  cycle <= 0;                  repeat( 99) @(posedge clk);
  oe <= 1;                                 @(posedge clk); // Ожидаем на шине флаг invalid, так как после RST новых загрузок небыло.  
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hC0C1C2C3; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hC4C5C6C7; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  for ( i = 0; i < BUF_SIZE; i = i + 1 )
    begin
      $display("%d -> %h , %h , %h, %h", i, pu.receive_buffer.memory[i], pu.transfer_out_buffer.memory[i], pu.transfer_in_buffer.memory[i], pu.send_buffer.memory[i]);
    end


  /////////////////// Четвёртый цикл.
  cycle <= 1;                              @(posedge clk); // сигнал о начале второго цикла
  cycle <= 0;                  repeat( 99) @(posedge clk);
  oe <= 1;                                 @(posedge clk);  // В буфере ожидаем: A0A1A2A3A4A5A6A7
                                                            // На шине A0A1A2A3
  oe <= 0;                     repeat( 10) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hC8C9CACB; @(posedge clk);
  wr <= 0;                     repeat( 48) @(posedge clk);
  wr <= 1; pu_spi_data_in <= 32'hCCCDCECF; @(posedge clk);
  wr <= 0;                     repeat( 40) @(posedge clk);

  $display("Buffers dump receive, transfer (data_out), tarnsfer (data_in), send:");
  for ( i = 0; i < BUF_SIZE; i = i + 1 )
    begin
      $display("%d -> %h , %h , %h, %h", i, pu.receive_buffer.memory[i], pu.transfer_out_buffer.memory[i], pu.transfer_in_buffer.memory[i], pu.send_buffer.memory[i]);
    end


end


initial begin // spi communication
  @(negedge rst);

  repeat(35) @(posedge clk); 
  // Во входном буфере должно появиться два слова: A1A2A3A4 и A5A6A7A8.
  // В канал должно уйти два слова из выходного буфера.

  master_in = 64'hA0A1A2A3A4A5A6A7;                        @(posedge clk); 
  start_transaction = 1;                                   @(posedge clk);
  start_transaction = 0;                                   @(posedge clk);
  // Из канала при этом ожидаем получить значение по умолчанию, а именно -
  // hCCCCCCCC, так как буфер не был заполнен для отправки.
  repeat(130) @(posedge clk); 
  repeat(35) @(posedge clk); 
  


  repeat(35) @(posedge clk); 
  // Во входном буфере должно появиться два слова: A9AAABAC и ADAEAFA0. 
  // В канал должно уйти два слова из выходного буфера.

  master_in = 64'hA8A9AAABACADAEAF;                        @(posedge clk);
  start_transaction = 1;                                   @(posedge clk);
  start_transaction = 0;                                   @(posedge clk);
  // Из канала при этом ожидаем получить значение hB0B1B2B3B4B5B6B7
  repeat(130 - 3) @(posedge clk); 
  repeat(35) @(posedge clk); 



  repeat(35) @(posedge clk); 
  // Во входном буфере должно появиться два слова: A9AAABAC и ADAEAFA0. 
  // В канал должно уйти два слова из выходного буфера.

  master_in = 64'hA8A9AAABACADAEAF;                        @(posedge clk);
  start_transaction = 1;                                   @(posedge clk);
  start_transaction = 0;                                   @(posedge clk);
  // Из канала при этом ожидаем получить значение hB0B1B2B3B4B5B6B7
  repeat(130 - 3) @(posedge clk); 
  repeat(35) @(posedge clk); 
  


  repeat(35) @(posedge clk); 
  // Во входном буфере должно появиться два слова: A9AAABAC и ADAEAFA0. 
  // В канал должно уйти два слова из выходного буфера.

  master_in = 64'hA8A9AAABACADAEAF;                        @(posedge clk);
  start_transaction = 1;                                   @(posedge clk);
  start_transaction = 0;                                   @(posedge clk);
  // Из канала при этом ожидаем получить значение hB0B1B2B3B4B5B6B7
  repeat(130 - 3) @(posedge clk); 
  repeat(35) @(posedge clk); 



  repeat(60) @(posedge clk); 

  $finish;
end


endmodule