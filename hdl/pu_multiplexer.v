module pu_multiplexer #(
    parameter DATA_WIDTH = 32,
    parameter ATTR_WIDTH = 4,
    parameter SEL_WIDTH = 1
)(
    input wire clk,
    input wire rst,
    input wire data_active,
    input wire sel_active,
    input wire out_active,
    
    input wire signed [DATA_WIDTH-1:0] data_in,
    input wire [ATTR_WIDTH-1:0] attr_in,
    output wire signed [DATA_WIDTH-1:0] data_out,
    output wire [ATTR_WIDTH-1:0] attr_out
);
    reg signed [DATA_WIDTH-1:0] buffer [0:(2**SEL_WIDTH)-1];
    reg [SEL_WIDTH-1:0] sel_reg;
    reg [SEL_WIDTH-1:0] write_index = 0;
    reg is_prev_out = 0;


    integer i;

    always @(posedge clk)
     if ( rst ) begin
       for (i = 0; i < (2**SEL_WIDTH); i = i + 1) begin
            buffer[i] <= 0;
        end
        write_index <= 0;
        sel_reg <= 0;
        is_prev_out <= 0;
     end

    always @(posedge clk) begin
        if (data_active) begin
            if (is_prev_out) begin
                sel_reg <= 0;
                is_prev_out <= 0;
                buffer[0] <= data_in;
                write_index <= 1;
            end else begin
                buffer[write_index] <= data_in;
                write_index <= write_index + 1;
            end
        end
        
        if (sel_active) begin
            if (is_prev_out) begin
                write_index <= 0;
                is_prev_out <= 0;
            end
            sel_reg <= data_in;
        end

        if (out_active) begin
            is_prev_out <= 1;
        end
    end

assign attr_out = 0;
assign data_out = (out_active && (sel_reg < write_index))? buffer[sel_reg] : 0;

endmodule
