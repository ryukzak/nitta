module pu_lut 
    #(  parameter DATA_WIDTH = 32,
        parameter ATTR_WIDTH = 4,
        parameter SEL_WIDTH = 4,
        parameter MAX_NUM_ARGS = 2,
        parameter LUT_DUMP = "dump/lut.hex"
    ) ( 
    input wire clk,
    input wire signal_oe,
    input wire signal_wr,
    input wire [SEL_WIDTH-1:0] signal_sel,

    input wire [DATA_WIDTH-1:0] data_in,
    input wire [ATTR_WIDTH-1:0] attr_in,
    output wire [DATA_WIDTH-1:0] data_out,
    output wire [ATTR_WIDTH-1:0] attr_out
);

    reg [MAX_NUM_ARGS-1:0] arg = 0;
    reg [$clog2(MAX_NUM_ARGS) - 1:0] arg_sel = 0;
    // reg [$clog2(MAX_NUM_ARGS) - 1:0] arg_sel = MAX_NUM_ARGS - 1;
    localparam ADDR_WIDTH = SEL_WIDTH + MAX_NUM_ARGS;
    reg memory [0:(1 << ADDR_WIDTH) - 1];
    wire [ADDR_WIDTH-1:0] addr;
    reg is_prev_out = 0;

    initial $readmemb(LUT_DUMP, memory);

    always @(posedge clk) begin
         if (signal_wr) begin
            $display("LUT: write %0d to arg_sel %0d", data_in, arg_sel);
            if (is_prev_out) begin
                arg <= 0;
                is_prev_out <= 0;
            end
            arg[arg_sel] <= data_in;
            arg_sel <= arg_sel + 1;
            // arg_sel <= arg_sel - 1;
        end
        if (signal_oe) begin
            arg_sel <= 0;
            // arg_sel <= MAX_NUM_ARGS - 1;
            is_prev_out <= 1;
            $display("LUT: read %0d from addr %0b", memory[addr], addr);
        end
    end

    assign addr = {signal_sel, arg};
    assign attr_out = attr_in;
    assign data_out = signal_oe ? memory[addr] : 0;

endmodule


