module spi_buffer
  #( parameter DATA_WIDTH     = 32
  ,  parameter SPI_DATA_WIDTH = 8
  ,  parameter BUF_SIZE       = 6
  )
  ( input                           clk
  , input                           rst
  , input      [SPI_DATA_WIDTH-1:0] data_in
  , output reg [SPI_DATA_WIDTH-1:0] data_out
  , input                           ready
  );

localparam ADDR_WIDTH  = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1];
reg [DATA_WIDTH-1:0]        next_word_to_send;
reg [ADDR_WIDTH-1:0]        address;
reg                         state;
reg                         addr_incremented;

localparam STATE_START_READY = 0;
localparam STATE_STOP_READY  = 1;

always @(posedge clk or posedge rst) begin
  if ( rst ) begin
    address <= 0;
    addr_incremented <= 0;
    state <= STATE_START_READY;
  end
  else begin
    if ( ready ) begin
      case ( state )
        STATE_START_READY: begin
          next_word_to_send <= memory [ address ]; // Save in the first line
          memory [ address + 1 ] <= data_in;	// Write in the second line
          state <= STATE_STOP_READY;
        end
        STATE_STOP_READY: begin
          data_out <= next_word_to_send;
          state <= STATE_START_READY;
        end
      endcase
    end
    if ( ready ) begin
      if ( !addr_incremented ) begin
        addr_incremented <= 1;
        address <= address + 1;
      end
    end else begin
      addr_incremented <= 0;
    end
  end
end

endmodule
