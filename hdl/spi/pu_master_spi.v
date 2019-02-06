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
    , input                     start_transaction

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

pu_slave_spi #
  ( .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
  , .BUF_SIZE( BUF_SIZE )
  , .BOUNCE_FILTER( 0 )
  ) pu
  ( .clk( clk )
  , .rst( rst )
  , .signal_cycle( signal_cycle )

  , .signal_wr( signal_wr )
  , .data_in( data_in )
  , .attr_in( attr_in )

  , .signal_oe( signal_oe )
  , .data_out( data_out )
  , .attr_out( attr_out )

  , .mosi( miso )
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
  , .start_transaction( start_transaction )
  , .cs( cs )
  , .sclk( sclk )
  );

endmodule
