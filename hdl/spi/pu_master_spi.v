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

spi_master_driver #
    ( .DATA_WIDTH( 32 )
    ) spi_driver
    ( .clk( clk )
    );

// pu_slave_spi_driver #
//         ( .DATA_WIDTH( SPI_DATA_WIDTH )
//         ) spi_driver
//     ( .clk( clk )
//     , .rst( rst )
//     , .data_in( splitter_to_spi )
//     , .data_out( splitter_from_spi )
//     , .ready( spi_ready )
//     , .prepare( spi_prepare )
//     , .mosi( f_mosi )
//     , .miso( miso )
//     , .sclk( f_sclk )
//     , .cs( f_cs )
//     );



endmodule
