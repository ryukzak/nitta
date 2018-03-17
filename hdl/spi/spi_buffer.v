module spi_buffer
  #( parameter DATA_WIDTH      = 32
    , parameter SPI_DATA_WIDTH = 8
    , parameter BUF_SIZE       = 10
   )
  ( input                        clk
   , input                       rst

   , input						 send
   , input						 receive 

   , input                       wr
   , input                       nitta_wr  
   , input      [DATA_WIDTH-1:0] data_in
   , input      [DATA_WIDTH-1:0] data_in_nitta

   , input                       oe 
   , input                       nitta_oe  
   , output reg [DATA_WIDTH-1:0] data_out
   , output reg [DATA_WIDTH-1:0] data_out_nitta

   , output     [SPI_DATA_WIDTH-1:0] spi_data_send
   , input      [SPI_DATA_WIDTH-1:0] spi_data_receive
   , input                           spi_ready
  );

localparam ADDR_WIDTH  = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory       [0:BUF_SIZE-1]; // [!] For transfer buffer : data_out
reg [DATA_WIDTH-1:0] memory_nitta [0:BUF_SIZE-1]; // [!] For transfer buffer : data_in
reg [ADDR_WIDTH-1:0]          oe_address; 
reg [ADDR_WIDTH-1:0]          wr_address;
reg [ADDR_WIDTH-1:0]          spi_address_recv;
reg [ADDR_WIDTH-1:0]          spi_address_send;
reg [ADDR_WIDTH-1:0]          nitta_wr_address;
reg [ADDR_WIDTH-1:0]          nitta_oe_address;
reg oe_addr_incremented;													// 32 BIT FRAME
reg wr_addr_incremented;													// 32 BIT FRAME
reg spi_addr_incremented_oe;											// 8 BIT SPI
reg spi_addr_incremented_wr;											// 8 BIT SPI
reg [SPI_DATA_WIDTH-1:0]      spi_next_word_to_send;
assign spi_data_send = spi_next_word_to_send;

reg [SPI_DATA_WIDTH-1:0]        spi_debug;
reg [DATA_WIDTH-1:0]            wr_debug;
reg [2:0] count_frame;
reg [2:0] count_frame_send;

localparam STATE_SPI_WAIT    = 0;
localparam STATE_START_READY = 1;
localparam STATE_STOP_READY  = 2;

reg [1:0] spi_state;
reg [1:0] spi_state_send;

always @(posedge clk or posedge rst) begin
	if ( rst ) begin
		oe_address <= 0;
		wr_address <= 0;

		nitta_wr_address <= 0;
		nitta_oe_address <= 0;

		spi_address_recv <= 0;
		spi_address_send <= 0;		
		
		spi_addr_incremented_oe <= 0;
		spi_addr_incremented_wr <= 0;
		spi_state <= STATE_SPI_WAIT;
		spi_state_send <= STATE_SPI_WAIT;
		spi_next_word_to_send <= 0;
		count_frame <= 0;
		count_frame_send <= 0;
	end
	else begin

		// store data for send
		if ( wr ) begin
			if ( !spi_addr_incremented_wr ) begin
				if ( nitta_wr ) begin
					memory_nitta[ nitta_wr_address ] <= data_in_nitta;
					nitta_wr_address <= nitta_wr_address + 1;
				end else begin
					memory[ wr_address ] <= data_in;
					wr_address <= wr_address + 1;
				end
				spi_addr_incremented_wr <= 1;
			end 
		end else if ( spi_ready && receive ) begin
			case ( spi_state )
				STATE_START_READY: begin
					memory[ spi_address_recv ] <= { memory[ spi_address_recv ][23:0], spi_data_receive };
					spi_state <= STATE_STOP_READY;
				end
			endcase
		end
		else begin 
			spi_addr_incremented_wr <= 0;
			case ( spi_state )
				STATE_STOP_READY: begin		
					count_frame <= count_frame + 1;			
					spi_state <= STATE_SPI_WAIT;
				end
				STATE_SPI_WAIT: begin		
					spi_state <= STATE_START_READY;  
				end
				default: spi_state <= STATE_SPI_WAIT;
			endcase
			if ( count_frame == 3'b100 ) begin 
				count_frame <= 0;
				spi_address_recv <= spi_address_recv + 1;
			end
		end

		// fetch received data
		if ( oe ) begin
			if ( !spi_addr_incremented_oe ) begin
				if ( nitta_oe ) begin
					data_out_nitta <= memory_nitta[ nitta_oe_address ];
					nitta_oe_address <= nitta_oe_address + 1;
				end else begin
					data_out <= memory[ oe_address ];
					oe_address <= oe_address + 1;
				end
				spi_addr_incremented_oe <= 1;
			end
		end else if ( spi_ready && send ) begin
			case ( spi_state_send )
				STATE_START_READY: begin
					spi_next_word_to_send <= memory [ spi_address_send ] >> SPI_DATA_WIDTH * count_frame_send;
					spi_state_send <= STATE_STOP_READY;
				end
			endcase
		end else begin
			spi_addr_incremented_oe <= 0;
			case ( spi_state_send )
				STATE_STOP_READY: begin		
					count_frame_send <= count_frame_send + 1;			
					spi_state_send <= STATE_SPI_WAIT;
				end
				STATE_SPI_WAIT: begin		
					spi_state_send <= STATE_START_READY;  
				end
				default: spi_state <= STATE_START_READY;
			endcase
			if ( count_frame_send == 3'b100 ) begin 
				count_frame_send <= 0;
				spi_address_send <= spi_address_send + 1;
			end

		end
	end
end

endmodule
