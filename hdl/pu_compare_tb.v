module pu_compare_tb();

    parameter DATA_WIDTH = 32;
    parameter ATTR_WIDTH = 4;
    parameter SEL_WIDTH = 3;

    reg clk;
    reg rst;
    reg wr, oe;
    reg [DATA_WIDTH-1:0] data_in;
    reg [ATTR_WIDTH-1:0] attr_in;
    reg [SEL_WIDTH-1:0] op_sel;

    wire [DATA_WIDTH-1:0] data_out;
    wire [ATTR_WIDTH-1:0] attr_out;

    localparam 
        TB_CMP_EQ  = 3'b000,
        TB_CMP_LT  = 3'b001,
        TB_CMP_LTE = 3'b010,
        TB_CMP_GT  = 3'b011,
        TB_CMP_GTE = 3'b100;

    pu_compare #(
        .DATA_WIDTH(DATA_WIDTH),
        .ATTR_WIDTH(ATTR_WIDTH),
        .SEL_WIDTH(SEL_WIDTH)
    ) dut (
        .clk(clk),
        .rst(rst),
        .wr(wr),
        .oe(oe),
        .data_in(data_in),
        .attr_in(attr_in),
        .op_sel(op_sel),
        .data_out(data_out),
        .attr_out(attr_out)
    );

    initial begin
        clk = 0;
        forever #1 clk <= ~clk;
    end
    
    initial begin
        rst <= 1;
        repeat(3) @(posedge clk);
        rst <= 0;
    end

    task automatic test_operation;
        input [SEL_WIDTH-1:0] op;
        input [DATA_WIDTH-1:0] a, b;
        input expected;
        begin

            @(posedge clk);
            wr <= 1;
            data_in <= a;
            op_sel <= op;
            
            @(posedge clk);
            wr <= 1;
            data_in <= b;

            @(posedge clk);
            wr <= 0;
            
             @(posedge clk);
            oe <= 1;
            repeat( 2 ) @(posedge clk);
            if (data_out !== expected)
                $display("FAIL: %0d %s %0d: expected %0d, got %0d",
                    a, get_op_name(op), b, expected, data_out);
            else
                $display("PASS: %0d %s %0d", a, get_op_name(op), b);
            oe <= 0;
            repeat(2) @(posedge clk);
        end
    endtask

    function [23:0] get_op_name(input [2:0] op);
        case(op)
            TB_CMP_EQ:  get_op_name = "==";
            TB_CMP_LT:  get_op_name = "< ";
            TB_CMP_LTE: get_op_name = "<=";
            TB_CMP_GT:  get_op_name = "> ";
            TB_CMP_GTE: get_op_name = ">=";
            default: get_op_name = "??";
        endcase
    endfunction

    initial begin
        $dumpfile("pu_compare_tb.vcd");
        $dumpvars(0, pu_compare_tb);

        wr <= 0;
        oe <= 0;
        data_in <= 0;
        attr_in <= 0;
        op_sel <= 0;
        repeat(2) @(posedge clk);

        test_operation(TB_CMP_EQ,  10, 10, 1);
        test_operation(TB_CMP_EQ,  100, 200, 0);
        test_operation(TB_CMP_LT,  5, 10, 1);
        test_operation(TB_CMP_LT,  10, 5, 0);
        test_operation(TB_CMP_GT,  20, 15, 1);
        test_operation(TB_CMP_GT,  15, 20, 0);
        test_operation(TB_CMP_LTE, 10, 10, 1);
        test_operation(TB_CMP_GTE, 10, 10, 1);
        
        repeat(2) @(posedge clk);
        $finish;
    end
endmodule
