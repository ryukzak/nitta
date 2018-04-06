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
, output                    flag_stop

// SPI interface
, input                     mosi
, output                    miso
, input                     sclk
, input                     cs
);

`include "parameters.vh"

wire [SPI_DATA_WIDTH-1:0] spi_data_send;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready;

// ---------- ATTR OUT BUFFER ------------
wire [ATTR_WIDTH-1:0] attr_out_transfer_in;
wire [ATTR_WIDTH-1:0] attr_out_transfer_out;
wire [ATTR_WIDTH-1:0] attr_out_send;
wire [ATTR_WIDTH-1:0] attr_out_receive;
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
// --------------- END -------------------


spi_slave_driver #( .DATA_WIDTH( SPI_DATA_WIDTH ) 
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

// [TRANSFER <<< NITTA]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
) transfer_in_buffer 
    ( .clk( clk )
    , .rst( rst )
    , .wr( signal_wr && ~work_buffer_send )
    , .attr_out( attr_out_transfer_in )
    , .data_in( transfer_in_data_in )
); 

// [TRANSFER >>> NITTA]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
) transfer_out_buffer 
    ( .clk( clk )
    , .rst( rst )

); 

// [MASTER >>> SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
            , .DATA_WIDTH( DATA_WIDTH )
) receive_buffer
    ( .clk( clk )
    , .rst( rst )

);

// [MASTER <<< SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
            , .DATA_WIDTH( DATA_WIDTH )
) send_buffer 
    ( .clk( clk )
    , .rst( rst )
    , .wr( signal_wr && work_buffer_send )
    , .attr_out( attr_out_send )
    , .data_in( send_data_in )
 );

hoarder frame_hoarder (
    .clk( clk )
,   .rst( rst )

);

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
//assign attr_out[INVALID] = 

endmodule
