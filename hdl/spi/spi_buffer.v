module spi_buffer
  #( parameter DATA_WIDTH     = 32
   , parameter SPI_DATA_WIDTH = 8
   , parameter BUF_SIZE       = 10
   )
  ( input                       clk
  , input                       rst

  , input                       wr  
  , input      [DATA_WIDTH-1:0] data_in

  , input                       oe  
  , output reg [DATA_WIDTH-1:0] data_out

  , output                      buffer_full

  , output      [SPI_DATA_WIDTH-1:0] spi_data_send
  , input       [SPI_DATA_WIDTH-1:0] spi_data_receive
  , input                           spi_ready
  );

localparam ADDR_WIDTH  = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1];
reg [ADDR_WIDTH-1:0]          oe_address; 
reg oe_addr_incremented;
reg [ADDR_WIDTH:0]            wr_address; // +1 на переполнение
reg wr_addr_incremented;
reg [ADDR_WIDTH-1:0]          spi_address;
reg spi_addr_incremented;
reg [SPI_DATA_WIDTH-1:0]        spi_next_word_to_send;
reg [SPI_DATA_WIDTH-1:0]        spi_debug;
assign spi_data_send = spi_next_word_to_send;

localparam STATE_START_READY = 0;
localparam STATE_STOP_READY  = 1;
reg spi_state;

integer index;
initial begin
  // Вынес, так как данный код либо не синтезируемый, либо очень много места займёт.
  for (index = 0; index < BUF_SIZE; index = index + 1) begin
    memory[ index ] = 0;
  end
end

always @(posedge clk or posedge rst) begin
  if ( rst ) begin
    oe_address <= 0;
    oe_addr_incremented <= 0;

    wr_address <= 0;
    wr_addr_incremented <= 0;

    spi_address <= 0;
    spi_addr_incremented <= 0;
    spi_state <= STATE_START_READY;
    spi_next_word_to_send <= 0;
  end
  else begin

    // store data for send
    if ( wr ) begin
      memory [ wr_address ] <= data_in;
      wr_addr_incremented = 1;
      wr_address <= { 1'b0, wr_address[ ADDR_WIDTH-1:0 ]}; 
    end else begin 
      if ( wr_addr_incremented ) begin
        wr_address <= wr_address + 1'b1;
        wr_addr_incremented = 0;
      end
    end

    // send and reseive data by SPI
    if ( spi_ready ) begin
      case ( spi_state )
        STATE_START_READY: begin
          spi_next_word_to_send <= memory[ spi_address ]; // Save in the first line
          memory[ spi_address + 1 ] <= spi_data_receive;	// Write in the second line
          spi_state <= STATE_STOP_READY;
        end
        STATE_STOP_READY: begin
          spi_state <= STATE_START_READY;
        end
      endcase

      if ( !spi_addr_incremented ) begin
        spi_addr_incremented <= 1;
        spi_address <= spi_address + 1;
      end
    end else begin
      spi_addr_incremented <= 0;
    end

    // fetch received data
    if ( oe ) begin
      data_out <= memory [ oe_address ];
      oe_addr_incremented <= 1;
    end else begin
      if ( oe_addr_incremented ) begin
        oe_address <= oe_address + 1'b1;
        oe_addr_incremented = 0;
      end
    end
  end
end

assign buffer_full = { wr_address [ADDR_WIDTH] };

endmodule
