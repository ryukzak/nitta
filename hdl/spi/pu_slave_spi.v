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
  , output                    flag_stop

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
wire spi_ready;

spi_slave_driver 
  #( .DATA_WIDTH( SPI_DATA_WIDTH ) 
   , .SUB_FRAME( 1 )
   ) spi_driver 
  ( .clk( clk )
  , .rst( rst )  
  , .data_in( splitter_to_spi ) 
  , .data_out( spi_data_receive )  
  , .ready( spi_ready )
  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );


// Преобразование слов процессора (DATA_WIDTH) в слова SPI (SPI_DATA_WIDTH).
wire splitter_ready;
wire [DATA_WIDTH-1:0] nitta_to_splitter;
nitta_to_spi_splitter 
  #( .DATA_WIDTH( DATA_WIDTH )
   , .ATTR_WIDTH( ATTR_WIDTH )
   , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
   ) nitta_to_spi_splitter 
  ( .clk( clk )
  , .rst( rst || flag_stop )

  , .spi_ready( spi_ready )
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

reg cs_prev;
always @(posedge clk) cs_prev <= cs;

reg wait_flag_start;
assign flag_start = cs ^ cs_prev && !cs 
                 && wait_flag_start;
always @( posedge clk ) begin
  if      ( rst || flag_stop ) wait_flag_start <= 1;
  else if ( flag_start       ) wait_flag_start <= 0;
end

// Необходимо что бы flag_stop сбрасывался сам через такт после установки.
assign flag_stop = cs ^ cs_prev && cs 
                && wait_flag_stop;
reg wait_flag_stop;
always @( posedge clk ) begin
  if      ( flag_start       ) wait_flag_stop <= 1;
  else if ( flag_stop || rst ) wait_flag_stop <= 0;
end


// FIXME:
assign data_out = 0;
assign attr_out = 0;

endmodule