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
wire spi_ready;
reg flag;

reg [7:0] counter;

always @(posedge clk_200MHz) begin
	if (!keys[0]) begin
		buffer <= 0;
		flag <= 1;
		counter <= 0;
	end else begin
		if ( spi_ready && flag == 0 ) begin
			buffer <= spi_data_receive;
			counter <= counter + 1;
			flag <= 1;
		end else if ( !spi_ready && flag == 1 ) flag <= 0;
	end
end

assign leds = keys[1] ? buffer : counter;

spi_slave_driver 
  #( .DATA_WIDTH( SPI_DATA_WIDTH ) 
   ) spi_driver 
  ( .clk( clk_200MHz )
  , .rst( !keys[0] ) 
  , .data_in( buffer ) 
  , .data_out( spi_data_receive )  
  , .ready( spi_ready )
  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );
               
endmodule