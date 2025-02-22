module pu_lut 
    #(  parameter ADDR_WIDTH = 4,
        parameter DATA_WIDTH = 32,
        parameter ATTR_WIDTH = 4,
        parameter SEL_WIDTH = 1, // should drop?
        parameter LUT_DUMP = "dump/lut.hex"
    ) ( input wire clk
    , input wire signal_oe
    , input wire signal_wr
    , input  wire [SEL_WIDTH-1:0] signal_sel

    , input  wire [DATA_WIDTH-1:0] data_in
    , input  wire [ATTR_WIDTH-1:0] attr_in
    , output reg  [DATA_WIDTH-1:0] data_out
    , output reg  [ATTR_WIDTH-1:0] attr_out
    );
    reg [DATA_WIDTH-1:0] memory [0:(1<<ADDR_WIDTH)-1];

    initial $readmemb(LUT_DUMP, memory);

    always @(posedge clk)
        if ( ~signal_oe ) { attr_out, data_out } <= 0;
        else              { attr_out, data_out } <= memory[data_in];

endmodule
