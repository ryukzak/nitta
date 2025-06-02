module pu_multiplexer_tb();

    parameter DATA_WIDTH = 32;
    parameter ATTR_WIDTH = 4;
    parameter SEL_WIDTH = 1;

    reg clk;
    reg signal_wr;
    reg signal_sel;
    reg signal_oe;
    reg [DATA_WIDTH-1:0] data_in;
    reg [ATTR_WIDTH-1:0] attr_in;
    wire [DATA_WIDTH-1:0] data_out;
    wire [ATTR_WIDTH-1:0] attr_out;

    pu_multiplexer #(
        .DATA_WIDTH(DATA_WIDTH),
        .ATTR_WIDTH(ATTR_WIDTH),
        .SEL_WIDTH(SEL_WIDTH))
    dut (
        .clk(clk),
        .signal_wr(signal_wr),
        .signal_sel(signal_sel),
        .signal_oe(signal_oe),
        .data_in(data_in),
        .attr_in(attr_in),
        .data_out(data_out),
        .attr_out(attr_out)
    );

    initial begin
        clk = 0;
        forever #1 clk = ~clk;
    end

    task automatic write_data;
        input [SEL_WIDTH-1:0] sel;
        input [DATA_WIDTH-1:0] data;
        input [ATTR_WIDTH-1:0] attr;
        begin
            @(posedge clk);
            signal_sel <= 1;
            data_in <= sel;
            @(posedge clk);
            signal_sel <= 0;
            
            signal_wr <= 1;
            data_in <= data;
            attr_in <= attr;
            @(posedge clk);
            signal_wr <= 0;
            $display("[%0t] Write: sel=%0d, data=0x%h, attr=0x%h", $time, sel, data, attr);
        end
    endtask

    task automatic check_output;
        input [SEL_WIDTH-1:0] expected_sel;
        input [DATA_WIDTH-1:0] expected_data;
        input [ATTR_WIDTH-1:0] expected_attr;
        begin
            @(posedge clk);
            signal_oe <= 1;
            @(posedge clk);
            if (data_out !== expected_data || attr_out !== expected_attr)
                $display("FAIL: sel=%0d, expected (0x%h, 0x%h), got (0x%h, 0x%h)",
                        expected_sel, expected_data, expected_attr, data_out, attr_out);
            else
                $display("PASS: sel=%0d, data=0x%h, attr=0x%h",
                        expected_sel, data_out, attr_out);
            signal_oe <= 0;
            @(posedge clk);
        end
    endtask

    initial begin
        $dumpfile("pu_multiplexer_tb.vcd");
        $dumpvars(0, pu_multiplexer_tb);

        signal_wr = 0;
        signal_sel = 0;
        signal_oe = 0;
        data_in = 0;
        attr_in = 0;
        
        write_data(0, 32'hAAAA_AAAA, 4'hA);
        write_data(1, 32'h5555_5555, 4'h5);
        
        write_data(0, 0, 0);
        check_output(0, 32'hAAAA_AAAA, 4'hA);
        
        write_data(1, 0, 0);
        check_output(1, 32'h5555_5555, 4'h5);

        @(posedge clk);
        signal_oe <= 0;
        @(posedge clk);
        if (data_out !== 0 || attr_out !== 0)
            $display("FAIL: Output not reset");
        else
            $display("PASS: Output reset");

        write_data(0, 32'hDEAD_BEEF, 4'hF);
        check_output(0, 32'hDEAD_BEEF, 4'hF);

        #100;
        $finish;
    end

endmodule
