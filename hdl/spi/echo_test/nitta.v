module nitta
    ( input external_clk

    , input [1:0] keys
    , output [7:0] leds

    , input mosi
    , output miso
    , input sclk
    , input cs
    ); 
  
 
parameter SPI_DATA_WIDTH = 8;
wire clk_200MHz, clk_5kHz;

wire rst = !keys[0];
assign leds = buffer;



pll pll
    ( .inclk0( external_clk )
    , .c0( clk_200MHz )
    , .c1( clk_5kHz )
    );



wire spi_ready, prepare;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
reg [SPI_DATA_WIDTH-1:0] buffer;


wire f_mosi, f_cs, f_sclk;
bounce_filter #( .DIV(20) ) ( rst, clk_200MHz, mosi, f_mosi );
bounce_filter #( .DIV(20) ) ( rst, clk_200MHz, cs,   f_cs );
bounce_filter #( .DIV(20) ) ( rst, clk_200MHz, sclk, f_sclk );

pu_slave_spi_driver 
  #( .DATA_WIDTH( SPI_DATA_WIDTH ) 
   ) spi_driver 
  ( .clk( clk_200MHz )
  , .rst( rst ) 
  , .data_in( spi_ready ? spi_data_receive : buffer ) 
  , .data_out( spi_data_receive )  
  , .ready( spi_ready )
  , .prepare( prepare ) // not used
  , .mosi( f_mosi )
  , .miso( miso )
  , .sclk( f_sclk )
  , .cs( f_cs )
  );


always @(posedge clk_200MHz) begin
    if ( rst ) begin
        buffer <= 0;
    end else begin
        if ( spi_ready ) begin
            buffer <= spi_data_receive;
        end
    end
end

endmodule