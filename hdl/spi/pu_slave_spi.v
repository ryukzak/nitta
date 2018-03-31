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

reg  resolution_wr;
reg  resolution_oe;
reg  start_load;
reg  switch;
reg  switch_send;
reg  buffer_rst;
reg  start_transaction;

wire [SPI_DATA_WIDTH-1:0] spi_data_send;
wire [SPI_DATA_WIDTH-1:0] spi_data_receive;
wire spi_ready;

reg  [SPI_DATA_WIDTH-1:0] data;

reg signal_wr_transfer_to_send;
reg uploading_data;

wire [DATA_WIDTH-1:0] transfer_in_out;
wire [DATA_WIDTH-1:0] send_out;
wire [DATA_WIDTH-1:0] receive_in;
wire [DATA_WIDTH-1:0] transfer_out_in;

wire [ATTR_WIDTH-1:0] attr_out_send;
wire [ATTR_WIDTH-1:0] attr_out_transfer_in;
wire [ATTR_WIDTH-1:0] attr_out_transfer_out;
wire [ATTR_WIDTH-1:0] attr_out_receive;
wire [ATTR_WIDTH-1:0] attr_hoarder_receive;

wire [DATA_WIDTH-1:0] data_out_nitta_receive;
wire [DATA_WIDTH-1:0] data_out_nitta_transfer;
wire [DATA_WIDTH-1:0] data_out_hoarder_receive;
wire [DATA_WIDTH-1:0] hoarder_data_in;
wire [SPI_DATA_WIDTH-1:0] data_out_byte;

localparam START_LOAD = 0;
localparam WAIT_TOOK  = 1;
localparam UPLOAD     = 2;
reg [1:0] state_mr; // master >>> receive 

spi_slave_driver #( .DATA_WIDTH( SPI_DATA_WIDTH ) ) spi_driver // SPI_DATA_WIDTH
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
	, .rst( rst )
	, .wr( signal_wr && switch_send )
	, .data_in( data_in ) 
	, .attr_out( attr_out_transfer_in )
    , .data_out( transfer_in_out )
	, .oe ( flag_start && !switch_send )
); 

// [TRANSFER >>> NITTA]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						) transfer_out_buffer 
	( .clk( clk )
	, .rst( rst || ( buffer_rst && !switch ) )
	, .wr( start_load && !switch )
	, .data_in( transfer_out_in )
	, .oe( signal_oe && switch )
	, .data_out( data_out_nitta_transfer )
	, .attr_out( attr_out_transfer_out )

); 

// [MASTER >>> SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						, .DATA_WIDTH( DATA_WIDTH )
						, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
						) receive_buffer
	( .clk( clk )
	, .rst( rst || ( buffer_rst && switch ))
	, .wr( start_load && switch )
	, .data_in( receive_in )
	, .oe( signal_oe && !switch )
	, .data_out( data_out_nitta_receive )
	, .attr_out( attr_out_receive )
);

// [MASTER <<< SLAVE]
spi_buffer #( .BUF_SIZE( BUF_SIZE )
						, .DATA_WIDTH( DATA_WIDTH )
						, .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
						) send_buffer 
	( .clk( clk )
	, .rst( rst )
	, .wr( signal_wr && !switch_send )
	, .data_in( data_in ) 
	, .attr_out( attr_out_send )
	, .data_out( send_out )
	// , .oe ( signal_wr_transfer_to_send && resolution_wr )
 );

hoarder receive (
	.clk( clk )
,	.rst( rst )
	// Receive
,	.ready( spi_ready )
,	.data_in_byte( spi_data_receive )
,	.oe( uploading_data )
,	.data_out( data_out_hoarder_receive )
,	.attr_hoarder( attr_hoarder_receive )
	// Send
,	.data_in( hoarder_data_in )
,	.wr( flag_start )
,	.data_out_byte( spi_data_send )
);



always @( posedge clk or posedge rst ) begin
	if ( rst ) begin
		start_load <= 0;
		switch <= 1;
		switch_send <= 1;
		state_mr <= START_LOAD;
	end else begin
		if ( signal_cycle ) begin

			// [+] Receive 
			if ( attr_out_transfer_out[1] == 1 ) begin
				buffer_rst <= 1;
				switch = ~switch;
			end else if ( attr_out_receive[1] == 1 ) begin
				buffer_rst <= 1;
				switch = ~switch;
			end

			// [+] Send
			if ( attr_out_send[1] == 1 ) begin
				switch_send = ~switch_send;
			end else if ( attr_out_transfer_in[1] == 1 ) begin
				switch_send = ~switch_send;
			end

		end else begin
			buffer_rst <= 0;
			start_transaction <= 0;
		end
	end
end

always @( negedge cs ) begin
	start_transaction <= 1;
end

// always @( posedge clk ) begin
// 	if ( spi_ready ) begin
// 		// reset
// 		data <= spi_data_receive;
// 	end
// end


always @(posedge clk ) begin
	if ( flag_start ) begin
		signal_wr_transfer_to_send <= 1;
	end else begin
		signal_wr_transfer_to_send <= 0;
	end
end


// always @( posedge clk ) begin
// 	if ( attr_out_send[1] == 1 && !signal_wr && flag_stop ) begin
// 		resolution_wr <= 0;
// 	end else if ((attr_out_transfer_in[1] == 1) && !signal_wr && flag_stop ) begin
// 		resolution_wr <= 1;
// 	end
// end



always @(posedge clk ) begin
	case ( state_mr )
		START_LOAD: begin
			if ( flag_stop ) begin
				uploading_data <= 1;
				state_mr <= WAIT_TOOK;
			end else begin
				start_load <= 0;
				uploading_data <= 0;
			end
		end
		WAIT_TOOK: begin
			if ( attr_hoarder_receive[0] == 1 ) begin
				state_mr <= UPLOAD;
			end
		end
		UPLOAD: begin
			start_load <= 1;
			state_mr <= START_LOAD;
		end
	endcase
end

assign receive_in      =  switch ? data_out_hoarder_receive : 0;
assign transfer_out_in = !switch ? data_out_hoarder_receive : 0;
assign data_out = switch ? data_out_nitta_transfer : data_out_nitta_receive;
assign flag_start = start_transaction;
assign flag_stop  = !spi_ready && cs;
assign hoarder_data_in = switch_send ?  send_out : transfer_in_out;
//assign spi_data_send = switch_send ? send_out : transfer_in_out ;

endmodule
