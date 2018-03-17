module pu_slave_spi
	#( parameter DATA_WIDTH     = 32
	 , parameter ATTR_WIDTH     = 4
	 , parameter SPI_DATA_WIDTH = 8
	 , parameter BUF_SIZE       = 6
	 )
	( input             		clk
	, input             		rst
	, input             		signal_cycle

	// system interface
	, input                     signal_wr
    , input   [DATA_WIDTH-1:0]  data_in
    , input   [ATTR_WIDTH-1:0]  attr_in

	, input                    	signal_oe
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
wire receive_to_pu_ready;

reg signal_wr_transfer_to_send;
wire [DATA_WIDTH-1:0] transfer_data_out_nitta;

 
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

// Туда сюда [SLAVE <-> NITTA]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
) transfer_buffer 
	( .clk( clk )
	, .rst( buffer_rst )
	, .wr( signal_wr )
	, .nitta_wr ( 1'b1 )
	, .nitta_oe ( 1'b1 )
	, .spi_ready( 1'b0 )
	, .oe( signal_oe )
	, .data_in_nitta( data_in )
	, .data_out_nitta( transfer_data_out_nitta )
); 

// Буфер на прием [MASTER -> SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
			, .DATA_WIDTH( DATA_WIDTH )
			, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
) receive_buffer
	( .clk( clk )
	, .rst( buffer_rst )
	, .wr( 1'b0 )
	, .nitta_wr( 1'b0 )
	, .nitta_oe( 1'b0 )
	, .receive( 1'b1 )
	, .send( 1'b0 )
	, .oe( signal_oe )
	, .spi_ready( spi_ready && !cs )
	, .spi_data_receive( spi_data_receive )
);

// Буфер на передачу [SLAVE -> MASTER]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
			, .DATA_WIDTH( DATA_WIDTH )
			, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
) send_buffer 
	( .clk( clk )
	, .rst( buffer_rst )
	, .wr( signal_wr_transfer_to_send )
	, .data_in( transfer_data_out_nitta )
	, .nitta_wr( 1'b0 )
	, .nitta_oe ( 1'b0 )
	, .oe ( 1'b0 )
	, .receive( 1'b0 )
	, .send( 1'b1 )
	, .spi_ready( spi_ready && !cs )
	, .spi_data_send( spi_data_send )
 );

always @( posedge clk or posedge rst ) begin
	if ( rst ) begin
		buffer_rst <= 1;
	end else begin
		if ( signal_cycle ) begin
			buffer_rst <= 1;
		end else begin                      
			buffer_rst <= 0;
		end                                 
	end
end

always @( posedge clk ) begin
	if ( signal_oe ) begin
		signal_wr_transfer_to_send <= 1;
	end else begin
		signal_wr_transfer_to_send <= 0;
	end
end

endmodule
