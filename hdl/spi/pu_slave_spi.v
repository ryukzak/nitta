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

  // system interface
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

// The attributes of the buffer
parameter INVALID        = 0;
parameter VALID          = 1;
parameter SPI_FINISH     = 2;
parameter FULL           = 3;

wire [SPI_DATA_WIDTH-1:0] spi_data_send;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready;

// ---------- ATTR OUT BUFFER ------------
wire [ATTR_WIDTH-1:0] attr_out_transfer_in;
wire [ATTR_WIDTH-1:0] attr_out_transfer_out;
wire [ATTR_WIDTH-1:0] attr_out_send;
wire [ATTR_WIDTH-1:0] attr_out_receive;
wire [ATTR_WIDTH-1:0] splitter_attr;
// --------------- END -------------------

// ---------- BUFFER SWITCHING -----------
localparam TRANSFER_IN = 0;
localparam SEND        = 1;
reg work_buffer_send;
// --------------- END -------------------

// ----------- WIRE DATA IN --------------
wire [DATA_WIDTH-1:0] transfer_in_data_in;
wire [DATA_WIDTH-1:0] transfer_out_data_in;
wire [DATA_WIDTH-1:0] send_data_in;
wire [DATA_WIDTH-1:0] receive_data_in;
wire [DATA_WIDTH-1:0] nitta_to_splitter;
// --------------- END -------------------

// ----------- WIRE DATA OUT -------------
wire [DATA_WIDTH-1:0] transfer_in_data_out;
wire [DATA_WIDTH-1:0] transfer_out_data_out;
wire [DATA_WIDTH-1:0] send_data_out;
wire [DATA_WIDTH-1:0] receive_data_out;
wire [DATA_WIDTH-1:0] hoarder_data_out;
// --------------- END -------------------

wire test_load;
reg flag;

spi_slave_driver 
  #( .DATA_WIDTH( SPI_DATA_WIDTH ) 
   ) spi_driver 
  ( .clk( clk )
  , .rst( rst )  
  , .data_in( spi_data_send ) 
  , .data_out( spi_data_receive )  
  , .ready( spi_ready )
  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );

wire splitter_ready;

nitta_to_spi_splitter nitta_to_spi_splitter 
  ( .clk( clk )
  , .rst( rst )

  , .spi_ready( spi_ready )
  , .to_spi( spi_data_send )

  , .splitter_ready( splitter_ready )
  , .from_nitta( nitta_to_splitter )
  // , .from_nitta( 32'hABCDEF42 )
  );

assign nitta_to_splitter = work_buffer_send ? transfer_in_data_out : send_data_out;


// [TRANSFER <<< NITTA]
spi_buffer 
  #( .BUF_SIZE( BUF_SIZE )
   ) transfer_in_buffer 
  ( .clk( clk )
  , .rst( rst )
  , .wr( signal_wr && ~work_buffer_send )
  , .attr_out( attr_out_transfer_in )
  , .data_in( transfer_in_data_in )
  // ------------------------------------
  , .oe( ( splitter_ready ) && work_buffer_send )
  , .data_out( transfer_in_data_out )
  ); 

// [TRANSFER >>> NITTA]
spi_buffer 
  #( .BUF_SIZE( BUF_SIZE )
   ) transfer_out_buffer 
  ( .clk( clk )
  , .rst( rst )
  , .wr( 1'b0 )
  , .oe( 1'b0 )
  , .data_in( 0 )
  ); 

// [MASTER >>> SLAVE]
spi_buffer
  #( .BUF_SIZE( BUF_SIZE )
   , .DATA_WIDTH( DATA_WIDTH )
   ) receive_buffer
  ( .clk( clk )
  , .rst( rst )
  , .wr( 1'b0 )
  , .oe( 1'b0 )
  , .data_in( 0 )
  );

// [MASTER <<< SLAVE]
spi_buffer 
  #( .BUF_SIZE( BUF_SIZE )
   , .DATA_WIDTH( DATA_WIDTH )
   ) send_buffer 
  ( .clk( clk )
  , .rst( rst )
  , .wr( signal_wr && work_buffer_send )
  , .attr_out( attr_out_send )
  , .data_in( send_data_in )
  // ----------------------------------
  , .oe( ( splitter_ready ) && ~work_buffer_send )
  , .data_out( send_data_out )
  );



always @( posedge clk ) begin
  if ( rst ) begin
    flag <= 1;
  end else if ( !cs && spi_ready && flag ) begin
    flag <= 0;
  end else if ( stop ) begin
    flag <= 1;
  end
end

always @( posedge clk or posedge rst ) begin
  if ( rst ) begin
    work_buffer_send <= SEND;
  end else begin
    if ( signal_cycle && cs ) begin
      if ( ~attr_out_transfer_in[ INVALID ] && attr_out_send[ INVALID ] ) begin
        work_buffer_send <= SEND;
      end else if ( ~attr_out_send[ INVALID ] && attr_out_transfer_in[ INVALID ] ) begin
        work_buffer_send <= TRANSFER_IN;
      end
    end else if ( ~signal_cycle ) begin
      if ( signal_oe ) begin

      end
    end
  end
end

assign { transfer_in_data_in, send_data_in } = work_buffer_send ? { 32'h00000000, data_in } : { data_in , 32'h00000000 };
assign flag_start = !cs && spi_ready && flag ? 1 : 0;

// Необходимо что бы flag_stop сбрасывался сам через такт после установки.
wire stop = !spi_ready && cs;
reg flag_stop_prev;
always @(posedge clk) begin
  if      ( rst )                         { flag_stop_prev, flag_stop } <= { 1'b0, 1'b0 };
  else if ( !flag_stop_prev && stop )     { flag_stop_prev, flag_stop } <= { 1'b1, 1'b1 };
  else if ( flag_stop_prev && flag_stop ) { flag_stop_prev, flag_stop } <= { 1'b1, 1'b0 };
  else                                    { flag_stop_prev, flag_stop } <= { 1'b0, 1'b0 };
end

assign data_out = 0;
assign attr_out = 0;

endmodule