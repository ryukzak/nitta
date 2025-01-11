module pu_lut 
    #(  parameter ADDR_WIDTH = 4,
        parameter DATA_WIDTH = 1,
        parameter LUT_DUMP = "dump/lut.hex"
    ) (
    input wire [ADDR_WIDTH-1:0] addr,
    output reg [DATA_WIDTH-1:0] data
    );
    reg [DATA_WIDTH-1:0] memory [0:(1<<ADDR_WIDTH)-1];

    initial $readmemb(LUT_DUMP, memory);

    always @(*) begin
        data = memory[addr];
    end
endmodule
