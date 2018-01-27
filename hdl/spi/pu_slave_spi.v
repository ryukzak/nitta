module pu_slave_spi
  #( parameter DATA_WIDTH     = 32
  ,  parameter ATTR_WIDTH     = 4
  ,  parameter SPI_DATA_WIDTH = 8
  ,  parameter BUF_SIZE       = 6
  )
  ( input             clk
  , input             rst
  , input             signal_cycle

  // system interface
  , input  wire                  signal_wr
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out

  , output            flag_start
  , output            flag_stop

  // SPI interface
  , input             mosi
  , output            miso
  , input             sclk
  , input             cs
  );


wire [SPI_DATA_WIDTH-1:0] spi_data_send;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready;

 
spi_slave_driver #( .DATA_WIDTH( SPI_DATA_WIDTH )
                  ) pu_spi_inner 
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

spi_buffer #( .BUF_SIZE( BUF_SIZE )
            ) buffer
  ( .clk( clk )
  , .rst( rst )
  , .data_in( spi_data_receive )
  , .data_out( spi_data_send )
  , .ready( spi_ready )
  );


always @(posedge clk or posedge rst) begin
  data_out <= 0;
  attr_out <= 0;
end


endmodule
