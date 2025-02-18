module tb_pu_lut;

    localparam ADDR_WIDTH = 4;
    localparam DATA_WIDTH = 1;
    localparam SEL_WIDTH = 2;
    localparam LUT_DUMP = "dump/lut.hex";

    reg clk;
    reg [ADDR_WIDTH-1:0] addr;
    reg signal_wr;
    reg signal_oe;
    reg [SEL_WIDTH-1:0] sel;
    wire [DATA_WIDTH-1:0] data;

    pu_lut #(
        .ADDR_WIDTH(ADDR_WIDTH),
        .DATA_WIDTH(DATA_WIDTH),
        .SEL_WIDTH(SEL_WIDTH),
        .LUT_DUMP(LUT_DUMP)
    ) uut (
        .clk(clk),
        .addr(addr),
        .signal_wr(signal_wr),
        .signal_oe(signal_oe),
        .sel(sel),
        .data(data)
    );

    initial begin
        clk = 0;
        forever #5 clk = ~clk;
    end

    initial begin
        addr = 0;
        signal_wr = 0;
        signal_oe = 0;
        sel = 0;

        #10;

        for (int i = 0; i < (1<<ADDR_WIDTH); i = i + 1) begin
            addr = i;
            signal_oe = 1;
            #10;
            $display("Reading addr: %d, data: %b", addr, data);
            signal_oe = 0;
        end

        #50;
        $stop;
    end
endmodule
