`timescale 1 ms/ 1 ms
module pu_slave_spi
  #( parameter DATA_WIDTH     = 32
   , parameter ATTR_WIDTH     = 4
   , parameter SPI_DATA_WIDTH = 8
   , parameter BUF_SIZE       = 6
   )
  ( input                     clk
  , input                     rst
  , input                     signal_cycle

  // nitta interface
  , input                     signal_wr
  , input    [DATA_WIDTH-1:0] data_in
  , input    [ATTR_WIDTH-1:0] attr_in

  , input                     signal_oe
  , output   [DATA_WIDTH-1:0] data_out
  , output   [ATTR_WIDTH-1:0] attr_out

  , output                    flag_start
  , output reg                flag_stop

  // SPI interface
  , input                     mosi
  , output                    miso
  , input                     sclk
  , input                     cs
  );


///////////////////////////////////////////////////////////
// [TRANSFER <<< NITTA]

// SPI
wire [SPI_DATA_WIDTH-1:0] splitter_to_spi;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready, spi_premature_ready;

spi_slave_driver 
  #( .DATA_WIDTH( SPI_DATA_WIDTH ) 
   , .SUB_FRAME( 1 )
   ) spi_driver 
  ( .clk( clk )
  , .rst( rst )  
  , .data_in( splitter_to_spi ) 
  , .data_out( spi_data_receive )  
  , .ready( spi_ready )
  , .premature_ready( spi_premature_ready )
  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );


// Преобразование слов процессора (DATA_WIDTH) в слова SPI (SPI_DATA_WIDTH).
wire splitter_ready;
wire [DATA_WIDTH-1:0] nitta_to_splitter;
nitta_to_spi_splitter nitta_to_spi_splitter 
  ( .clk( clk )
  , .rst( rst || flag_stop )

  , .spi_ready( spi_premature_ready )
  , .to_spi( splitter_to_spi )

  , .splitter_ready( splitter_ready )
  , .from_nitta( nitta_to_splitter )
  );


// Переключение буферов отправки данных из процессора на интерфейс SPI.

reg buf_sel;
always @( posedge clk ) begin
  if ( rst ) buf_sel <= 0;
  else if ( signal_cycle && cs ) buf_sel <= !buf_sel;
end

wire n2s_wr [0:1];
assign n2s_wr[0] = signal_wr;
assign n2s_wr[1] = 1'b0;

wire [DATA_WIDTH-1:0] n2s_data_in [0:1];
assign n2s_data_in[0] = data_in;
assign n2s_data_in[1] = 1'b0;

wire n2s_oe [0:1];
assign n2s_oe[0] = 1'b0;
assign n2s_oe[1] = splitter_ready && !cs;

wire [DATA_WIDTH-1:0] n2s_data_out[0:1];
assign nitta_to_splitter = n2s_data_out[!buf_sel];

// Буфера на отправку
buffer
  #( .BUF_SIZE( BUF_SIZE )
   ) n2s_buffer0 // nitta to spi
  ( .clk( clk )
  , .rst( rst || flag_stop )

  , .wr( n2s_wr[buf_sel] )
  , .data_in( n2s_data_in[buf_sel] )

  , .oe( n2s_oe[buf_sel] )
  , .data_out( n2s_data_out[0] )
  ); 

buffer
  #( .BUF_SIZE( BUF_SIZE )
   ) n2s_buffer1 // nitta to spi
  ( .clk( clk )
  , .rst( rst || flag_stop )

  , .wr( n2s_wr[!buf_sel] )
  , .data_in( n2s_data_in[!buf_sel] )

  , .oe( n2s_oe[!buf_sel] )
  , .data_out( n2s_data_out[1] )
  ); 


///////////////////////////////////////////////////////////
// Флаги

assign flag_start = !cs && spi_ready && flag_start_prev;
reg flag_start_prev;
always @( posedge clk ) begin
  if      ( rst || stop ) flag_start_prev <= 1;
  else if ( flag_start )  flag_start_prev <= 0;
end

// Необходимо что бы flag_stop сбрасывался сам через такт после установки.
wire stop = !spi_ready && cs;
reg flag_stop_prev;
always @(posedge clk) begin
  if      ( rst )                         { flag_stop_prev, flag_stop } <= { 1'b0, 1'b0 };
  else if ( !flag_stop_prev && stop )     { flag_stop_prev, flag_stop } <= { 1'b1, 1'b1 };
  else if ( flag_stop_prev && flag_stop ) { flag_stop_prev, flag_stop } <= { 1'b1, 1'b0 };
  else                                    { flag_stop_prev, flag_stop } <= { 1'b0, 1'b0 };
end


// FIXME:
assign data_out = 0;
assign attr_out = 0;

endmodule