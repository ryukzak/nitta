module spi_buffer
	#( parameter DATA_WIDTH     = 32
	 , parameter SPI_DATA_WIDTH = 8
	 , parameter BUF_SIZE       = 10
	 , parameter ATTR_WIDTH     = 4
	 )
	( input                       clk
	, input                       rst

	, input                       wr
	, input      [DATA_WIDTH-1:0] data_in

	, input                       oe 
	, output reg [DATA_WIDTH-1:0] data_out

	, input      [ATTR_WIDTH-1:0] attr_in
	, output reg [ATTR_WIDTH-1:0] attr_out

	, output     [SPI_DATA_WIDTH-1:0] spi_data_send
	, input      [SPI_DATA_WIDTH-1:0] spi_data_receive
	, input                           spi_ready
	);

localparam ADDR_WIDTH  = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory       [0:BUF_SIZE-1]; // [!] For transfer buffer : data_out
reg [ADDR_WIDTH-1:0]          oe_address; 
reg [ADDR_WIDTH-1:0]          wr_address;
reg [ADDR_WIDTH-1:0]          spi_address_recv;
reg [ADDR_WIDTH-1:0]          spi_address_send;
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

		spi_address_recv <= 0;
		spi_address_send <= 0;		
		
		spi_addr_incremented_oe <= 0;
		spi_addr_incremented_wr <= 0;
		spi_state <= STATE_SPI_WAIT;
		spi_state_send <= STATE_SPI_WAIT;
		spi_next_word_to_send <= 0;
		count_frame <= 0;
		count_frame_send <= 0;
		attr_out <= 4'b0000;
	end
	else begin

		// store data for send

		if ( wr ) begin
			memory[ wr_address ] <= data_in;			
			wr_address <= wr_address + 1;
			attr_out[0] <= 1;
			attr_out[1] <= 0;
			attr_out[2] <= 0;
		end
		
		// fetch received data
		if ( oe && attr_out[0] ) begin
			data_out <= memory[ oe_address ];
			oe_address <= oe_address + 1;
		end
	
	end
end

always @(posedge clk ) begin
	if ( wr_address == 0 ) begin
		attr_out[1] = 1; // Буффер пуст
	end
end

// Ошибка! +1 
always @(posedge clk ) begin
	if ( (wr_address == oe_address) && !wr && !oe ) begin
		attr_out[2] <= 1; // Переданы все данные
		wr_address  <= 0;
		oe_address  <= 0;
	end
end

// attr_out[0] - ФЛАГ: Данные есть -> можно из буфера читать данные
// attr_out[1] - ФЛАГ: Данных нету -> буфер пустой, сделано для переключения буферов
// attr_out[2] - ФЛАГ: SPI закончил читать данные из буфера

endmodule
