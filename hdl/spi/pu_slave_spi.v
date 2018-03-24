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
reg  resolution_wr;
reg  resolution_oe;

wire [DATA_WIDTH-1:0] spi_data_send;
wire [DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready;

reg signal_wr_transfer_to_send;
reg signal_oe_to_nitta;

wire [DATA_WIDTH-1:0] transfer_in_out;
wire [DATA_WIDTH-1:0] send_out;
wire [DATA_WIDTH-1:0] receive_in;
wire [DATA_WIDTH-1:0] transfer_out_in;

wire [ATTR_WIDTH-1:0] attr_out_send;
wire [ATTR_WIDTH-1:0] attr_out_transfer_in;
wire [ATTR_WIDTH-1:0] attr_out_transfer_out;
wire [ATTR_WIDTH-1:0] attr_out_receive;

wire [DATA_WIDTH-1:0] data_out_nitta_receive;
wire [DATA_WIDTH-1:0] data_out_nitta_transfer;

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
	, .wr( signal_wr && resolution_wr )
	, .data_in( data_in ) 
	, .attr_out( attr_out_transfer_in )
	, .data_out( transfer_in_out )
	, .oe ( signal_wr_transfer_to_send && !resolution_wr )
); 

// [TRANSFER >>> NITTA]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						) transfer_out_buffer 
	( .clk( clk )
	, .rst( buffer_rst )
	, .data_in( transfer_out_in ) 
	, .data_out( data_out_nitta_transfer )
	, .attr_out( attr_out_transfer_out )
	, .oe( signal_oe_to_nitta && !resolution_oe )
	, .wr( signal_wr_transfer_to_send && resolution_oe )

); 

// [MASTER >>> SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						, .DATA_WIDTH( DATA_WIDTH )
						, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
						) receive_buffer
	( .clk( clk )
	, .rst( buffer_rst )
	, .oe( signal_oe_to_nitta && resolution_oe )
	, .data_in( receive_in )
	, .attr_out( attr_out_receive )
	, .data_out( data_out_nitta_receive )
	, .wr( signal_wr_transfer_to_send && !resolution_oe )

);

// [MASTER <<< SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						, .DATA_WIDTH( DATA_WIDTH )
						, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
						) send_buffer 
	( .clk( clk )
	, .rst( buffer_rst )
	, .wr( signal_wr && !resolution_wr )
	, .data_in( data_in ) 
	, .attr_out( attr_out_send )
	, .data_out( send_out )
	, .oe ( signal_wr_transfer_to_send && resolution_wr )
 );

always @( posedge clk or posedge rst ) begin
	if ( rst ) begin
		buffer_rst <= 1;
		resolution_wr <= 1;
		resolution_oe <= 1;
	end else begin
		if ( signal_cycle ) begin
			buffer_rst <= 1;
		end else begin  
			buffer_rst <= 0;
		end                                 
	end
end

always @( posedge clk ) begin
	if ( attr_out_send[1] == 1 && !signal_wr && flag_stop ) begin
		resolution_wr <= 0;
	end else if ((attr_out_transfer_in[1] == 1) && !signal_wr && flag_stop ) begin
		resolution_wr <= 1;
	end
end

always @( posedge clk ) begin
	if ( (attr_out_receive[1] == 1 || attr_out_receive[3] == 1 ) && !signal_oe && flag_stop ) begin
		resolution_oe <= 0;
	end else
	if (( (attr_out_transfer_out[1] == 1) || attr_out_transfer_out[3] == 1 ) && !signal_oe && flag_stop ) begin
		resolution_oe <= 1;
	end 
end

// add signal_oe_buff_to_nitta

always @(posedge clk ) begin
	if ( flag_stop ) begin
		signal_wr_transfer_to_send <= 1;
	end else begin
		signal_wr_transfer_to_send <= 0;
	end
end

always @(posedge clk ) begin
	if ( signal_oe ) begin
		signal_oe_to_nitta <= 1;
	end else begin
		signal_oe_to_nitta <= 0;
	end
end

assign spi_data_send =   !resolution_wr ? transfer_in_out : send_out;
assign transfer_out_in =  resolution_oe ? spi_data_receive : 0;
assign receive_in      = !resolution_oe ? spi_data_receive : 0;
assign data_out =        !resolution_oe ? data_out_nitta_transfer : data_out_nitta_receive ;
assign flag_start = spi_ready && !cs;
assign flag_stop  = !spi_ready && cs;


endmodule
