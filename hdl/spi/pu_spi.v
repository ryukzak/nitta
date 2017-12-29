module pu_spi
  #( parameter DATA_WIDTH = 8
 ,   parameter SCLK_HALFPERIOD = 1
   )
  ( input             clk
  , input             rst
  // system interface
  , input  [DATA_WIDTH-1:0] data_in
  , output [DATA_WIDTH-1:0] data_out
  , output                  ready
  // SPI interface
  , output wire       miso
  , output            mosi
  , input             sclk
  , input             cs
  );

assign slave_spi_mosi = mosi;
assign slave_spi_sclk = sclk;
assign slave_spi_cs   = cs;
assign slave_ready    = ready;
assign miso = slave_spi_miso;

spi_slave_driver pu_spi_inner ( 
    .clk(clk)
  , .rst(rst)  
  , .data_in(data_in)
  , .ready(slave_ready)
  , .data_out(data_out)  
  , .mosi(slave_spi_mosi)
  , .miso(slave_spi_miso)
  , .sclk(slave_spi_sclk)
  , .cs(slave_spi_cs)
  );

pu_buffer buffer (
    .clk(clk)
  , .rst(rst)
  , .ready(slave_ready)
  , .data_in(data_out)
  , .data_out(data_in)
);

endmodule
