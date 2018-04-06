module hoarder
#(
    parameter DATA_WIDTH     = 32
  , parameter ATTR_WIDTH     = 4
  , parameter SPI_DATA_WIDTH = 8
)
(
    input                           clk
,   input                           rst
,   input                           ready
,   input                           wr
,   input                           oe
,   input      [DATA_WIDTH-1:0]     data_in
,   output reg [DATA_WIDTH-1:0]     data_out
,   output reg [ATTR_WIDTH-1:0]     attr_hoarder
,   input      [SPI_DATA_WIDTH-1:0] data_in_byte
,   output     [SPI_DATA_WIDTH-1:0] data_out_byte
);

localparam SIZE_FRAME  = $clog2( DATA_WIDTH );
reg SEND, RECEIVE;

reg [DATA_WIDTH-1:0] frame [0:1];
reg [SIZE_FRAME-4:0] count [0:1];
reg [ATTR_WIDTH-1:0] attr  [0:1];

always @( posedge clk ) begin
    if ( rst ) begin
        SEND          <= 0;
        RECEIVE       <= 1;
        attr[  SEND ] <= 0;
        count[ SEND ] <= SIZE_FRAME - 2;
    end else begin

       // [+] send master
        if ( wr ) begin
            count[SEND] <= SIZE_FRAME - 2;
            frame[SEND] <= data_in;
        end else if ( ready && ~attr[SEND] ) begin            
            attr[SEND] <= 1;
        end else if ( !ready && attr[SEND] ) begin            
            count[SEND] <= count[SEND] - 1;
            attr[SEND] <= 0;
        end

    end
end

assign data_out_byte = count[SEND] == SIZE_FRAME - 2 ? data_in >> SPI_DATA_WIDTH * count[SEND] : frame[SEND] >> SPI_DATA_WIDTH * count[SEND];

endmodule