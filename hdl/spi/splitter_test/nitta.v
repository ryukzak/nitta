module nitta
  #( parameter SPI_DATA_WIDTH = 8
  , parameter DATA_WIDTH = 32
  )
  ( input external_clk

  , input [1:0] keys
  , output [7:0] leds

  , input mosi
  , output miso
  , input sclk
  , input cs
  );

wire clk_200MHz, clk_5kHz;

wire rst = !keys[0];

pll pll
    ( .inclk0( external_clk )
    , .c0( clk_200MHz )
    , .c1( clk_5kHz )
    );

wire spi_ready, prepare;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire [SPI_DATA_WIDTH-1:0] to_spi;
assign leds = to_spi;

wire f_mosi, f_cs, f_sclk;
bounce_filter #( .DIV(20) ) ( rst, clk_200MHz, mosi, f_mosi );
bounce_filter #( .DIV(20) ) ( rst, clk_200MHz, cs,   f_cs );
bounce_filter #( .DIV(20) ) ( rst, clk_200MHz, sclk, f_sclk );

pu_slave_spi_driver
  #( .DATA_WIDTH( SPI_DATA_WIDTH )
   ) spi_driver
  ( .clk( clk_200MHz )
  , .rst( rst )
  , .data_in( to_spi )
  , .prepare( prepare ) // not used
  , .mosi( f_mosi )
  , .miso( miso )
  , .sclk( f_sclk )
  , .cs( f_cs )
  );

wire splitter_ready;
wire [DATA_WIDTH-1:0] nitta_to_splitter;
nitta_to_spi_splitter #
        ( .DATA_WIDTH( DATA_WIDTH )
        , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
        ) nitta_to_spi_splitter
    ( .clk( clk_200MHz )
    , .rst( rst || f_cs )

    , .spi_ready( prepare )
    , .to_spi( to_spi )
    , .from_nitta( 'hAAABACAD )
    );

endmodule
