`timescale 1 ms/ 1 ms

// FIXME: Почистить реализацию модуля. Перенести модули тестового окружения в поддиректорию "test".

// FIXME: Необходимо сделать получение данных через данный вычислительный блок.

// FIXME: Необходимо сделать корректную работу с атрибутами (архитектура, testbench-и, испытания в железе).

module pu_slave_spi #
        ( parameter DATA_WIDTH     = 32
        , parameter ATTR_WIDTH     = 4
        , parameter SPI_DATA_WIDTH = 8
        , parameter BUF_SIZE       = 6
        , parameter BOUNCE_FILTER  = 20
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

    , output reg                flag_stop

    // SPI interface
    , input                     cs
    , input                     sclk
    , input                     mosi
    , output                    miso
    );


///////////////////////////////////////////////////////////
// [NITTA >>> SPI]



// send_buffers
reg send_buffer_sel;

wire send_buffer_wr[1:0];
wire send_buffer_oe[1:0];
wire [DATA_WIDTH-1:0] send_buffer_data_in[1:0];
wire [DATA_WIDTH-1:0] send_buffer_data_out[1:0];

generate
    genvar i;
    for ( i = 0; i < 2; i = i + 1 ) begin : send_buffer_i
        buffer #
                ( .BUF_SIZE( BUF_SIZE )
                ) send_buffer // from nitta to spi
            ( .clk( clk )
            , .rst( rst || flag_stop )

            , .wr( send_buffer_wr[i] )
            , .data_in( send_buffer_data_in[i] )

            , .oe( send_buffer_oe[i] )
            , .data_out( send_buffer_data_out[i] )
            ); 
    end
endgenerate

// signal_wr can be received only from the nitta's side.
assign send_buffer_wr[0] =  send_buffer_sel ? signal_wr : 1'h0;
assign send_buffer_wr[1] = !send_buffer_sel ? signal_wr : 1'h0;
assign send_buffer_data_in[0] =  send_buffer_sel ? data_in : 0;
assign send_buffer_data_in[1] = !send_buffer_sel ? data_in : 0;

// signal_oe can be received only from the spi driver's side (splitter).
assign send_buffer_oe[0] = !send_buffer_sel ? splitter_ready : 1'h0;
assign send_buffer_oe[1] =  send_buffer_sel ? splitter_ready : 1'h0;
wire [DATA_WIDTH-1:0] nitta_to_splitter =  send_buffer_data_out[send_buffer_sel];



// splitter: translate from DATA_WIDTH to SPI_DATA_WIDTH
wire splitter_ready;
wire [SPI_DATA_WIDTH-1:0] splitter_to_spi;
wire spi_prepare;
nitta_to_spi_splitter #
        ( .DATA_WIDTH( DATA_WIDTH )
        , .ATTR_WIDTH( ATTR_WIDTH )
        , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
        ) nitta_to_spi_splitter 
    ( .clk( clk )
    , .rst( rst || flag_stop )

    , .spi_ready( spi_prepare )
    , .to_spi( splitter_to_spi )

    , .splitter_ready( splitter_ready )
    , .from_nitta( nitta_to_splitter )
    );



// SPI driver
wire f_mosi, f_cs, f_sclk;

pu_slave_spi_driver #
        ( .DATA_WIDTH( SPI_DATA_WIDTH )
        ) spi_driver
    ( .clk( clk )
    , .rst( rst )
    , .data_in( splitter_to_spi )
    // , .data_out(  )
    // , .ready(  )
    , .prepare( spi_prepare )
    , .mosi( f_mosi )
    , .miso( miso )
    , .sclk( f_sclk )
    , .cs( f_cs )
    );

// bounce filter
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_mosi_filter ( rst, clk, mosi, f_mosi );
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_cs_filter   ( rst, clk, cs,   f_cs   );
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_sclk_filter ( rst, clk, sclk, f_sclk );



///////////////////////////////////////////////////////////
// Control logic

reg prev_f_cs;
always @( posedge clk ) prev_f_cs <= f_cs;

always @( posedge clk ) begin
    if ( rst ) send_buffer_sel <= 0;
    else if ( signal_cycle && f_cs ) send_buffer_sel <= !send_buffer_sel;
end

always @( posedge clk ) begin
    if ( rst ) flag_stop <= 0;
    else if ( !prev_f_cs && f_cs ) flag_stop <= 1;
    else flag_stop <= 0;
end

assign data_out = 0;
assign attr_out = 0;

endmodule