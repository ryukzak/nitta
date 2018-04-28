module hoarder
  #( parameter DATA_WIDTH     = 32
   , parameter ATTR_WIDTH     = 4
   , parameter SPI_DATA_WIDTH = 8
   // The attributes of the buffer
   , parameter INVALID        = 0
   , parameter VALID          = 1
   , parameter SPI_FINISH     = 2
   , parameter FULL           = 3
   )
  ( input                           clk
  , input                           rst
  , input                           ready
  , input                           flag_start
  , input                           wr
  , input                           oe
  , input      [DATA_WIDTH-1:0]     data_in
  , output reg [DATA_WIDTH-1:0]     data_out
  , output     [ATTR_WIDTH-1:0]     attr_hoarder
  , input      [SPI_DATA_WIDTH-1:0] data_in_byte
  , output     [SPI_DATA_WIDTH-1:0] data_out_byte
  );

localparam SIZE_FRAME  = $clog2( DATA_WIDTH );
reg SEND, RECEIVE, FLAG, PROCESS;

reg [DATA_WIDTH-1:0] frame [0:1];
reg [SIZE_FRAME-4:0] count [0:1];
reg [ATTR_WIDTH-1:0] attr  [0:1];

always @( posedge clk ) begin
  if ( rst ) begin
    SEND                <= 0;
    RECEIVE             <= 1;
    FLAG                <= 0;
    PROCESS             <= 1;
    attr[FLAG][SEND]    <= 0;
    attr[PROCESS][SEND] <= 1;
    count[ SEND ] <= SIZE_FRAME - 2;
  end else begin
    // [+] send master
    if ( wr ) begin
        count[SEND] <= SIZE_FRAME - 2;
        frame[SEND] <= data_in;
        attr[PROCESS][SEND] <= 1;
    end else if ( ready && ~attr[FLAG][SEND] && attr[PROCESS][SEND] ) begin            
        attr[FLAG][SEND] <= 1;
    end else if ( !ready && attr[FLAG][SEND] && attr[PROCESS][SEND] ) begin            
        count[SEND] <= count[SEND] - 1;
        attr[FLAG][SEND] <= 0;
        if ( count[SEND] == 0 ) begin
            attr[PROCESS][SEND] <= 0;
        end
    end
  end
end

assign data_out_byte = ( count[SEND] == SIZE_FRAME - 2 ) && flag_start ? data_in >> SPI_DATA_WIDTH * count[SEND] : frame[SEND] >> SPI_DATA_WIDTH * count[SEND];
assign { attr_hoarder[INVALID] , attr_hoarder[1], attr_hoarder[2], attr_hoarder[3] } = { count[SEND] == 0 && ready, 1'b0, 1'b0, 1'b0 };

endmodule