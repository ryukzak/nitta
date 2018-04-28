`timescale 1 ms/ 1 ms
module spi_buffer
  #( parameter DATA_WIDTH     = 32
   , parameter BUF_SIZE       = 10
   , parameter ATTR_WIDTH     = 4
   // The attributes of the buffer
   , parameter INVALID        = 0
   , parameter VALID          = 1
   , parameter SPI_FINISH     = 2
   , parameter FULL           = 3
   )
  ( input                       clk
  , input                       rst

  , input                       wr
  , input      [DATA_WIDTH-1:0] data_in

  , input                       oe 
  , output     [DATA_WIDTH-1:0] data_out

  , output reg [ATTR_WIDTH-1:0] attr_out
  );

localparam ADDR_WIDTH  = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1]; 
reg [ADDR_WIDTH-1:0]        oe_address; 
reg [ADDR_WIDTH-1:0]        wr_address;

reg [DATA_WIDTH-1:0]        wr_debug;

always @( posedge clk or posedge rst ) begin
  if ( rst ) begin
    oe_address <= 0;
    wr_address <= 0;
    attr_out <= 4'b0000;
  end
  else begin
    // store data for send
    if ( wr ) begin // Если хотим, чтоб после заполнения буффера 
                    // новые значения не перезаписывали 
            // еще не переданные, добавить в условие => && ~attr_out[ FULL ]
      memory[ wr_address ] <= data_in;      
      wr_address <= wr_address + 1;
      attr_out[ INVALID ] <= 0;
    end  else if ( wr_address == 0 ) begin
      attr_out[ INVALID ] <= 1;
      attr_out[ FULL ]    <= 0;
    end else if ( wr_address == oe_address ) begin
      attr_out[ INVALID ] <= 1;
      attr_out[ FULL ]    <= 0;
      wr_address <= 0;
      oe_address <= 0;
    end else if ( wr_address == BUF_SIZE ) begin
      attr_out[ FULL ] <= 1;
    end
    // fetch received data
    if ( oe && ~attr_out[ INVALID ] ) begin
      oe_address <= oe_address + 1;
    end
  end
end

assign data_out = oe && ~attr_out[ INVALID ] ? memory[ oe_address ] : 32'h00000000;

// attr_out[ INVALID ] => на шину выставляется логическая единица, для случая, когда у нас нету данных для передачи, или когда все данные переданы
// attr_out[ FULL ]    => на шину выставляется логическая еденица, когда буффер переполнен

endmodule