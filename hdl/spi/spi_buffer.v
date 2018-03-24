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

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1]; 
reg [ADDR_WIDTH-1:0]          oe_address; 
reg [ADDR_WIDTH-1:0]          wr_address;

reg [SPI_DATA_WIDTH-1:0]        spi_debug;
reg [DATA_WIDTH-1:0]            wr_debug;


always @(posedge clk or posedge rst) begin
	if ( rst ) begin
		oe_address <= 0;
		wr_address <= 0;
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
	if ( wr_address == 0 && !wr ) begin
		attr_out[1] <= 1; // Буффер пуст
		attr_out[0] <= 0;
	end
end

always @(posedge clk ) begin
	if ( (wr_address == oe_address) && !wr && !oe ) begin
		attr_out[2] <= 1; // Переданы все данные
		attr_out[3] <= 0;
		wr_address  <= 0;
		oe_address  <= 0;
	end
end

always @(posedge clk ) begin
	if ( wr_address == (BUF_SIZE-1) ) begin
		attr_out[3] <= 1; // Буффер заполнен
	end
end

// attr_out[0] - ФЛАГ: Данные есть -> можно из буфера читать данные
// attr_out[1] - ФЛАГ: Данных нету -> буфер пустой, сделано для переключения буферов
// attr_out[2] - ФЛАГ: SPI закончил читать данные из буфера
// attr_out[3] - ФЛАГ: Буфер заполнен

endmodule
