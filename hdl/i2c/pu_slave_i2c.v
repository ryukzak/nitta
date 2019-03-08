module pu_slave_i2c 
  #( parameter I2C_DATA_WIDTH = 8
   , parameter DATA_WIDTH     = 32
   , parameter ADDRES_DEVICE  = 7'h47
   )
  ( input                   clk
  , input                   rst

  // system interface
  , input  [I2C_DATA_WIDTH-1:0] data_in  
  , output [I2C_DATA_WIDTH-1:0] data_out 
  , output                      ready

  // i2c interface
  , input                   scl
  , inout                   sda 
  );

i2c_slave_driver #
  ( .DATA_WIDTH( DATA_WIDTH )
  , .I2C_DATA_WIDTH( I2C_DATA_WIDTH )
  , .ADDRES_DEVICE( ADDRES_DEVICE )
  ) driver_slave
  ( .clk( clk )
  , .rst( rst )
  , .data_in( 8'hDF )
  , .ready( ready )
  , .scl( scl )
  , .sda( sda )
  );

endmodule