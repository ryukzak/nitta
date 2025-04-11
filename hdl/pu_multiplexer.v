module pu_multiplexer #(
    parameter DATA_WIDTH = 32,
    parameter ATTR_WIDTH = 4,
    parameter SEL_WIDTH = 1
)(
    input wire clk,
    input wire data_active,
    input wire sel_active,
    input wire out_active,
    
    input wire [DATA_WIDTH-1:0] data_in,
    input wire [ATTR_WIDTH-1:0] attr_in,
    output reg [DATA_WIDTH-1:0] data_out,
    output reg [ATTR_WIDTH-1:0] attr_out
);
    reg [DATA_WIDTH-1:0] buffer [0:(2**SEL_WIDTH)-1];
    reg [SEL_WIDTH-1:0] sel_reg;

    always @(posedge clk) begin
        if (data_active) begin
            buffer[sel_reg] <= data_in;
        end
        
        if (sel_active) begin
            sel_reg <= data_in[SEL_WIDTH-1:0];
        end
    end

    always @(posedge clk) begin
        if (~out_active) { attr_out, data_out } <= 0;
        else { attr_out, data_out } <= buffer[sel_reg];
    end
endmodule
