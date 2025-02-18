module pu_lut 
    #(  parameter ADDR_WIDTH = 4,
        parameter DATA_WIDTH = 1,
        parameter SEL_WIDTH = 2,
        parameter LUT_DUMP = "dump/lut.hex"
    ) (
    input wire clk,
    input wire [ADDR_WIDTH-1:0] addr,
    input  wire  signal_wr,
    input wire signal_oe,
    input wire [SEL_WIDTH-1:0] sel,
    output reg [DATA_WIDTH-1:0] data
    );
    reg [DATA_WIDTH-1:0] memory [0:(1<<ADDR_WIDTH)-1];

    initial $readmemb(LUT_DUMP, memory);

    always @(posedge clk) begin
        data = memory[addr];
    end
endmodule
