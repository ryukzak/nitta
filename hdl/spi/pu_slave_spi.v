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

wire [DATA_WIDTH-1:0] spi_data_send;
wire [DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready;

reg signal_wr_transfer_to_send;
wire [DATA_WIDTH-1:0] transfer_in_out;

wire [ATTR_WIDTH-1:0] attr_out_send;
wire [ATTR_WIDTH-1:0] attr_out_transfer;

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

// [TRANSFER <<< NITTA]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						) transfer_in_buffer 
	( .clk( clk )
	, .rst( buffer_rst )
	, .wr( signal_wr )
	, .data_in( data_in )
	, .oe( flag_stop )
	, .data_out( transfer_in_out )

); 

// [TRANSFER >>> NITTA]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						) transfer_out_buffer 
	( .clk( clk )
	, .rst( buffer_rst )

); 

// [MASTER >>> SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						, .DATA_WIDTH( DATA_WIDTH )
						, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
						) receive_buffer
	( .clk( clk )
	, .rst( buffer_rst )

);

// [MASTER <<< SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						, .DATA_WIDTH( DATA_WIDTH )
						, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
						) send_buffer 
	( .clk( clk )
	, .rst( buffer_rst )
	, .wr ( signal_wr_transfer_to_send )
	, .data_in ( transfer_in_out )
	, .oe ( signal_wr_transfer_to_send )
	, .data_out( spi_data_send )

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

always @(posedge clk ) begin
	if ( flag_stop ) begin
		signal_wr_transfer_to_send <= 1;
	end else begin
		signal_wr_transfer_to_send <= 0;
	end
end

// assign flag_start = spi_ready && !cs;
assign flag_stop  = !spi_ready && cs;


endmodule
