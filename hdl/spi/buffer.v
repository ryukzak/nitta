`timescale 1 ms/ 1 ms
module buffer #
        ( parameter DATA_WIDTH = 32
        , parameter ATTR_WIDTH = 4
        , parameter BUF_SIZE   = 10
        , parameter FILE       = ""
        , parameter I = 0
        )
    ( input                   clk
    , input                   rst

    , input                   receive_mode
    , input                   action

    , input      [DATA_WIDTH-1:0] data_in
    , output reg [DATA_WIDTH-1:0] data_out
    );

localparam ADDR_WIDTH = $clog2( BUF_SIZE );

// for wr - current value
// for oe - previous value
reg [ADDR_WIDTH-1:0] addr;

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1];
reg prev_receive_mode;
always @( posedge clk ) prev_receive_mode <= receive_mode;

wire is_mode_changed = prev_receive_mode != receive_mode;

generate
    if ( FILE != "" ) begin
        initial $readmemh(FILE, memory, 0, BUF_SIZE-1);
    end
endgenerate

always @( posedge clk ) begin
    if ( rst | !action & is_mode_changed ) begin
        addr <= 0;
        data_out <= memory[ 0 ];
    end else if (  receive_mode ) begin
        if ( action & is_mode_changed ) begin
            // $display("%d> write to buffer %d %H (mode change)", I, 0, data_in);
            memory[ 0 ] <= data_in;
            addr <= 1;
            data_out <= memory[ 0 ];
        end else if ( action ) begin
            // $display("%d> write to buffer %d %H", I, addr, data_in);
            memory[ addr ] <= data_in;
            addr <= addr + 1;
        end
    end else if ( !receive_mode & action ) begin // if (  receive_mode )
        if ( action & is_mode_changed ) begin
            // $display("%d> read from buffer to out register %d %H", I, 0, memory[ 0 ]);
            data_out <= memory[ 0 ];
            addr <= 0;
        end else if ( action ) begin
            // $display("%d> read from buffer %d %H", I, addr + 1, memory[ addr ]);
            data_out <= memory[ addr + 1 ];
            addr <= addr + 1;
        end
    end
end

endmodule
