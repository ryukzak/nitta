`timescale 1 ms/ 1 ms
module bounce_filter_tb();

reg  clk;
reg  rst;
reg in1, in2;
wire out1, out2;

bounce_filter #
        ( .DIV( 0 )
        ) filter0
    ( .rst( rst )
    , .clk( clk )
    , .in( in1 )
    , .out( out1 )
    );

bounce_filter #
        ( .DIV( 2 )
        ) filter1
    ( .rst( rst )
    , .clk( clk )
    , .in( in2 )
    , .out( out2 )
    );

always begin
    clk <= 0;
    forever #1 clk <= ~clk;
end

initial begin
    rst <= 1;
    repeat(5) @(posedge clk);
    rst <= 0;
end

initial begin
    $dumpfile("bounce_filter_tb.vcd");
    $dumpvars(-1, bounce_filter_tb);
end

initial begin
    in1 <= 0;
    repeat(5) @(posedge clk);
    if (out1 !== 0) $display("FAIL filter0 #1");

    @(posedge clk);
    in1 = 1;
    if (out1 !== 1) $display("FAIL filter0 #2");

    @(posedge clk);
    in1 = 0;
    if (out1 !== 0) $display("FAIL filter0 #3");
    repeat(100) @(posedge clk); $finish;
end

initial begin
    in2 <= 0;
    repeat(5) @(posedge clk);
    if (out2 !== 0) $display("FAIL filter1 #1");

    in2 = 1;
    repeat(5) @(posedge clk);
    if (out2 !== 1) $display("FAIL filter1 #2");

    in2 = 0;
    repeat(5) @(posedge clk);
    if (out2 !== 0) $display("FAIL filter1 #3");

    in2 = 1;
    repeat(5) @(posedge clk);
    if (out2 !== 1) $display("FAIL filter1 #4");

    in2 = 0;
    repeat(3) begin
        @(posedge clk);
        if (out2 !== 1) $display("FAIL filter1 #5");
    end
    in2 = 1;
    repeat(3) begin
        @(posedge clk);
        if (out2 !== 1) $display("FAIL filter1 #6");
    end

    $finish;
end

endmodule
