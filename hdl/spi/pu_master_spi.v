`timescale 1 ms/ 1 ms

module pu_master_spi #
        ( parameter DATA_WIDTH     = 32
        , parameter ATTR_WIDTH     = 4
        , parameter SPI_DATA_WIDTH = 8
        , parameter BUF_SIZE       = 6
        , parameter BOUNCE_FILTER  = 4
        )
    ( input                     clk
    , input                     rst
    , input                     signal_cycle_begin
    , input                     signal_in_cycle
    , input                     signal_cycle_end

    // nitta interface
    , input                     signal_wr
    , input    [DATA_WIDTH-1:0] data_in
    , input    [ATTR_WIDTH-1:0] attr_in

    , input                     signal_oe
    , output   [DATA_WIDTH-1:0] data_out
    , output   [ATTR_WIDTH-1:0] attr_out

    , output reg                flag_stop

    // SPI interface
    , output                    cs
    , output                    sclk
    , input                     miso
    , output                    mosi
    );

reg disabled = 0;

wire f_miso;
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_mosi_filter ( rst, clk, miso, f_miso );

pu_slave_spi #
        ( .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
        , .DATA_WIDTH( DATA_WIDTH )
        , .BUF_SIZE( BUF_SIZE )
        , .BOUNCE_FILTER( 0 )
        ) pu
    ( .clk( clk )
    , .rst( rst )
    , .signal_cycle_begin( signal_cycle_begin )
    , .signal_in_cycle( signal_in_cycle )
    , .signal_cycle_end( signal_cycle_end )

    , .signal_wr( signal_wr )
    , .data_in( data_in )
    , .attr_in( attr_in )

    , .signal_oe( signal_oe )
    , .data_out( data_out )
    , .attr_out( attr_out )

    , .mosi( f_miso )
    , .miso( mosi )
    , .sclk( sclk )
    , .cs( cs )
    );

spi_master_driver #
        ( .DATA_WIDTH( DATA_WIDTH * 2 )
        , .SCLK_HALFPERIOD( 1 )
        ) master
    ( .clk( clk )
    , .rst( rst )
    , .start_transaction( signal_cycle_begin )
    , .cs( cs )
    , .sclk( sclk )
    );

reg prev_f_cs;
always @( posedge clk ) prev_f_cs <= cs;

always @( posedge clk ) begin
    if ( rst | !prev_f_cs && cs) flag_stop <= 1;
    else if ( disabled ) flag_stop <= 1;
    else if ( prev_f_cs && cs ) flag_stop <= 0;
    else flag_stop <= 0;
end

endmodule
