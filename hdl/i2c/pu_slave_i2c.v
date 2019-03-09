module pu_slave_i2c 
  #( parameter DATA_WIDTH     = 32
   , parameter ATTR_WIDTH     = 4
   , parameter I2C_DATA_WIDTH = 8
   , parameter BUF_SIZE       = 6
   , parameter BOUNCE_FILTER  = 4
   , parameter INVALID        = 0
   , parameter SIZE_WORDS     = 2
   , parameter ADDRES_DEVICE  = 7'h47
   )
  ( input                   clk
  , input                   rst
  , input                   signal_cycle

  // nitta interface

  , input                     signal_wr
  , input    [DATA_WIDTH-1:0] data_in
  , input    [ATTR_WIDTH-1:0] attr_in

  , input                     signal_oe
  , output   [DATA_WIDTH-1:0] data_out
  , output   [ATTR_WIDTH-1:0] attr_out

  , output reg                flag_stop

  // i2c interface
  , input                   scl
  , inout                   sda
    
  , output                  D0
  , output                  D1
  , output                  D2
  , output                  D3
  , output                  D4
  , output                  D5
  , output                  D6
  , output                  D7
  
  );

wire i2c_ready;

i2c_slave_driver #
  ( .DATA_WIDTH( DATA_WIDTH )
  , .I2C_DATA_WIDTH( I2C_DATA_WIDTH )
  , .ADDRES_DEVICE( ADDRES_DEVICE )
  ) driver_slave
  ( .clk( clk )
  , .rst( rst )
  , .data_in( 8'hAA )
  , .data_out( data_out )
  , .ready_read( i2c_ready )
  , .scl( scl )
  , .sda( sda )

  , .D0( D0 )
  , .D1( D1 )
  , .D2( D2 )
  , .D3( D3 )
  , .D4( D4 )
  , .D5( D5 )
  , .D6( D6 )
  , .D7( D7 )
  );



endmodule