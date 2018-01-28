module pu_spi
  #( parameter DATA_WIDTH       = 8
  ,  parameter ATTR_WIDTH       = 4
  ,  parameter VALID            = 1
  ,  parameter BUFFER_SIZE      = 8
   )
  ( input                   clk
  , input                   rst

  // system interface
  , input  wire                  signal_wr 
  , input  wire [DATA_WIDTH-1:0] data_in  
  , input  wire [ATTR_WIDTH-1:0] attr_in 

  , input  wire                  signal_oe 
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out 

  , input  wire       flag_cycle 
  , output reg        flag_start
  , output reg        flag_stop

  // SPI interface
  , output wire       miso
  , output wire       mosi
  , input  wire       sclk
  , input  wire       cs
);

localparam BUFFER_SIZE_WIDTH = $clog2( BUFFER_SIZE );
reg  [BUFFER_SIZE_WIDTH-1:0] actual_buffer_sending_size;
reg  [BUFFER_SIZE_WIDTH-1:0] actual_buffer_receiving_size;

wire [DATA_WIDTH-1:0] write_data_out;
reg  [DATA_WIDTH-1:0] write_data_in;
wire [DATA_WIDTH-1:0] read_data_out;
reg  [DATA_WIDTH-1:0] read_data_in;

reg  reset;

pu_buffer buffer_read (
  .clk(clk)
, .rst(reset)
, .oe(signal_oe)
//, .wr(signal_wr)
, .data_in(read_data_in)
, .data_out(read_data_out)
);

pu_buffer buffer_write (
  .clk(clk)
, .rst(reset)
//, .oe(signal_oe)
, .wr(signal_wr)
, .data_in(write_data_in)
, .data_out(write_data_out) 
); 

always @( posedge clk or posedge rst ) begin
  if ( rst ) begin
    reset <= 1;
  end else begin
    if ( flag_cycle ) begin
      reset <= 1;
    end else begin                      // [+] Start work basic transfer cycle
        if ( signal_oe ) begin          // [?] Logic for reading data
          data_out <= read_data_out;
        end
        if ( signal_wr ) begin          // [!] Logic for writing data
          write_data_in <= data_in;
        end    
      reset <= 0;
    end                                 // [+] End work basic transfer cycle
  end
end

endmodule
