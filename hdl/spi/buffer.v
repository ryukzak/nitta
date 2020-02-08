`timescale 1 ms/ 1 ms
module buffer #
        ( parameter DATA_WIDTH = 32
        , parameter ATTR_WIDTH = 4
        , parameter BUF_SIZE   = 10
        , parameter FILE       = ""
        )
    ( input                   clk
    , input                   rst

    , input                   wr
    , input  [DATA_WIDTH-1:0] data_in

    , input                   oe 
    , output reg [DATA_WIDTH-1:0] data_out
    );

localparam ADDR_WIDTH = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1]; 
reg [ADDR_WIDTH-1:0] addr;

generate
    if ( FILE != "" ) begin
        initial $readmemh(FILE, memory, 0, BUF_SIZE-1);
    end
endgenerate

always @( posedge clk ) begin
    if ( rst ) begin
        addr <= 0;
        data_out <= memory[ 0 ];
    end else if ( wr || oe ) begin
        if ( wr ) memory[ addr ] <= data_in;
        if ( oe ) data_out <= memory[ addr + 1 ];
        addr <= addr + 1;
    end
end

endmodule
