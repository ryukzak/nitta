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

wire [SPI_DATA_WIDTH-1:0] splitter_to_spi;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready, spi_premature_ready
;

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


wire splitter_ready;
wire [DATA_WIDTH-1:0] nitta_to_splitter;
nitta_to_spi_splitter nitta_to_spi_splitter 
  ( .clk( clk )
  , .rst( rst || flag_stop )

  , .spi_ready( spi_premature_ready )
  , .to_spi( splitter_to_spi )

  , .splitter_ready( splitter_ready )
  , .from_nitta( nitta_to_splitter )
  // , .from_nitta( 32'hABCDEF42 )
  );


wire                  n2s_wr = signal_wr;
wire [DATA_WIDTH-1:0] n2s_data_in = data_in;


// [TRANSFER <<< NITTA]
spi_buffer
  #( .BUF_SIZE( BUF_SIZE )
   ) n2s_buffer1 // nitta to spi
  ( .clk( clk )
  , .rst( rst || flag_sstop )

  , .wr( n2s_wr )
  , .data_in( n2s_data_in )

  , .oe( 1'b0 )
  // , .data_out( n2s_data_out )
  ); 

spi_buffer
  #( .BUF_SIZE( BUF_SIZE )
   ) n2s_buffer2 // nitta to spi
  ( .clk( clk )
  , .rst( rst || flag_stop )

  , .wr( 1'b0 )
  , .data_in( 32'h0 )

  , .oe( splitter_ready && !cs )
  , .data_out( nitta_to_splitter )
  ); 






reg flag; // WTF?
always @( posedge clk ) begin
  if ( rst ) begin
    flag <= 1;
  end else if ( !cs && spi_ready && flag ) begin
    flag <= 0;
  end else if ( stop ) begin
    flag <= 1;
  end
end

// always @( posedge clk or posedge rst ) begin
//   if ( rst ) begin
//     work_buffer_send <= SEND;
//   end else begin
//     if ( signal_cycle && cs ) begin
//       if ( ~attr_out_transfer_in[ INVALID ] && attr_out_send[ INVALID ] ) begin
//         work_buffer_send <= SEND;
//       end else if ( ~attr_out_send[ INVALID ] && attr_out_transfer_in[ INVALID ] ) begin
//         work_buffer_send <= TRANSFER_IN;
//       end
//     end else if ( ~signal_cycle ) begin
//       if ( signal_oe ) begin

//       end
//     end
//   end
// end

assign flag_start = !cs 
                 && spi_ready 
                 && flag;

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