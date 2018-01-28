module pu_slave_spi
  #( parameter DATA_WIDTH     = 32
   , parameter ATTR_WIDTH     = 4
   , parameter SPI_DATA_WIDTH = 8
   , parameter BUF_SIZE       = 6
   )
  ( input             clk
  , input             rst
  , input             signal_cycle

  // system interface
  , input                    signal_wr
  , input   [DATA_WIDTH-1:0] data_in
  , input   [ATTR_WIDTH-1:0] attr_in

  , input                    signal_oe
  , output   [DATA_WIDTH-1:0] data_out
  , output   [ATTR_WIDTH-1:0] attr_out

  , output            flag_start
  , output            flag_stop

  // SPI interface
  , input             mosi
  , output            miso
  , input             sclk
  , input             cs
  );

reg  buffer_rst;

wire [SPI_DATA_WIDTH-1:0] spi_data_send;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready;
 
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

spi_buffer #( .BUF_SIZE( BUF_SIZE )
            ) transfer_buffer 
  ( .clk( clk )
  , .rst( buffer_rst )
  , .spi_data_send( spi_data_send )
  , .spi_data_receive( spi_data_receive ) 
  , .spi_ready( spi_ready ) 
  ); 

spi_buffer #( .BUF_SIZE( BUF_SIZE )
            , .DATA_WIDTH( DATA_WIDTH )
            , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
            ) receive_buffer
  ( .clk( clk )
  , .rst( buffer_rst )
  , .oe( signal_oe )
  , .wr( 1'b0 )
  , .data_in( data_in )
  , .data_out( data_out )
  );

spi_buffer #( .BUF_SIZE( BUF_SIZE )
            , .DATA_WIDTH( DATA_WIDTH )
            , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
            ) send_buffer 
  ( .clk( clk )
  , .rst( buffer_rst )
  , .oe( 1'b0 )
  , .wr( signal_wr )
  , .data_in( data_in )
  // , .data_out( send_data_out ) 
  );

always @( posedge clk or posedge rst ) begin
  if ( rst ) begin
    buffer_rst <= 1;
  end else begin
    if ( signal_cycle ) begin
      buffer_rst <= 1;
    end else begin                      // [+] Start work basic transfer cycle
      buffer_rst <= 0;
    end                                 // [+] End work basic transfer cycle
  end
end

endmodule
