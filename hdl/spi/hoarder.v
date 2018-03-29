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
,   output reg [SPI_DATA_WIDTH-1:0] data_out_byte
);

localparam STATE_IDLE      = 0;
localparam STATE_SPI_START = 1;
localparam STATE_SPI_END   = 2;

reg [1:0] send_state;
reg [1:0] recv_state;
reg [DATA_WIDTH-1:0]     frame;
reg [DATA_WIDTH-1:0]     frame_send;
reg [SPI_DATA_WIDTH-1:0] byte;
reg [1:0] count_frame_send;
reg [1:0] count_frame_recv;

// Flags
reg took;
reg took_send;

always @( posedge clk ) begin
    if ( rst ) begin
        took <= 0;
        took_send <= 0;
        count_frame_send <= 0;
    end else begin

        // [+] receive master
        if ( oe && took ) begin
            attr_hoarder[0] <= 1;
            data_out <= frame;
        end else if ( ready && !took ) begin
            frame <= { frame[23:0], data_in_byte };
            took <= 1;
        end else if ( !ready && took ) begin
            took <= 0;
        end else begin
            attr_hoarder[0] <= 0;
        end

        // [+] send master
        if ( wr ) begin
            count_frame_send <= 0;
            frame_send <= data_in;
        end else if ( ready && !took_send) begin
            data_out_byte <= frame_send >> SPI_DATA_WIDTH * count_frame_send;
            count_frame_send <= count_frame_send + 1;
            took_send <= 1;
        end else if ( !ready && took_send) begin            
            if ( count_frame_send == 4 ) begin
                count_frame_send <= 0;
            end 
            took_send <= 0;
        end


    end
end

endmodule