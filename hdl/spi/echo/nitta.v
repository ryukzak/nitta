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


pll pll
    ( .inclk0( external_clk )
    , .c0( clk_200MHz )
    , .c1( clk_5kHz )
    );

reg [SPI_DATA_WIDTH-1:0] buffer;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready, prepare;
reg flag;

reg [7:0] counter;

wire rst = !keys[0];


always @(posedge clk_200MHz) begin
    if ( rst ) begin
        buffer <= 0;
        flag <= 1;
        counter <= 0;
    end else begin
        if ( prepare && flag == 0 ) begin
            buffer <= spi_data_receive;
            counter <= counter + 1;
            flag <= 1;
        end else if ( !prepare && flag == 1 ) begin
            flag <= 0;
        end
    end
end

assign leds = keys[1] ? buffer : counter;

wire f_mosi, f_cs, f_sclk;
bounce_filter #( .DIV(20)) ( rst, clk_200MHz, mosi, f_mosi );
bounce_filter #( .DIV(20)) ( rst, clk_200MHz, cs,   f_cs );
bounce_filter #( .DIV(20)) ( rst, clk_200MHz, sclk, f_sclk );

spi_slave_driver2 
  #( .DATA_WIDTH( SPI_DATA_WIDTH ) 
   ) spi_driver 
  ( .clk( clk_200MHz )
  , .rst( rst ) 
  , .data_in( buffer ) 
  , .data_out( spi_data_receive )  
  , .ready( spi_ready ), .prepare( prepare )
  , .mosi( f_mosi )
  , .miso( miso )
  , .sclk( f_sclk )
  , .cs( f_cs )
  );
               
endmodule