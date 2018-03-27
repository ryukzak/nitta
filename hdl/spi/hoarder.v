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
,   input      [SPI_DATA_WIDTH-1:0] data_in_byte
,   output reg [SPI_DATA_WIDTH-1:0] data_out_byte
);

localparam STATE_IDLE      = 0;
localparam STATE_SPI_START = 1;
localparam STATE_SPI_END   = 2;

reg [1:0] send_state;
reg [1:0] recv_state;
reg [DATA_WIDTH-1:0]     frame;
reg [SPI_DATA_WIDTH-1:0] byte;
reg [1:0] count_frame_send;
reg [1:0] count_frame_recv;

always @( posedge clk ) begin
    if ( rst ) begin
        count_frame_send <= 0;
        count_frame_recv <= 0;
        send_state <= STATE_IDLE;
    end else begin

        // [+] send master
        if ( wr ) begin
            frame <= data_in;
            send_state <= STATE_IDLE;
        end else begin
            if ( ready ) begin
                case ( send_state )
                    STATE_SPI_START: begin
                        data_out_byte <= frame >> SPI_DATA_WIDTH * count_frame_send;
                        send_state <= STATE_SPI_END;
                    end
                endcase
            end else begin
                case ( send_state )
                    STATE_IDLE: begin
                        if ( count_frame_send == 4 ) begin
                          count_frame_send <= 0;
                        end
                        send_state <= STATE_SPI_START;
                    end
                    STATE_SPI_END: begin
                        count_frame_send <= count_frame_send + 1;
                        send_state <= STATE_IDLE;
                    end
                endcase
            end
        end

        // [+] receive master
        if ( oe ) begin
            data_out <= frame;
            recv_state <= STATE_IDLE;
        end else begin
            if ( ready ) begin
                case ( recv_state )
                    STATE_SPI_START: begin
                        byte <= data_in_byte;
                        recv_state <= STATE_SPI_END;
                    end
                endcase
            end else begin
                case ( recv_state )
                    STATE_IDLE: begin
                        if ( count_frame_recv == 4 ) begin
                          count_frame_send <= 0;
                        end
                        recv_state <= STATE_SPI_START;
                    end
                    STATE_SPI_END: begin
                        frame = { frame[23:0], byte };
                        count_frame_recv <= count_frame_recv + 1;
                        recv_state <= STATE_IDLE;
                    end            
                endcase
            end
        end

    end
end

endmodule