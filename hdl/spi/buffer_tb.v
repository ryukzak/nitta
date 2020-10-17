`timescale 1 ms/ 1 ms
module buffer_tb();

reg  clk;
reg  rst;

reg receive_mode, action;
reg [31:0] data_in;
wire [31:0] data_out;

buffer #
        ( .DATA_WIDTH( 32 )
        , .ATTR_WIDTH( 0 )
        , .BUF_SIZE( 6 )
        ) buffer
    ( .clk( clk )
    , .rst( rst )

    , .receive_mode( receive_mode )
    , .action( action )
    , .data_in( data_in )
    , .data_out( data_out )
    );

always begin
    clk <= 1;
    rst <= 1;
    repeat(4) #5 clk <= ~clk;
    rst <= 0;
    forever #5 clk <= ~clk;
end

task assert32;
    input [31:0] a;
    input [31:0] b;
    begin
        if ( !(a === b) ) begin
            $display("ASSERT FAIL: %h === %h", a, b);
            $finish(2);
        end
    end
endtask

initial begin
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0;

    @(negedge rst);
    receive_mode <= 1; data_in <= 32'h00112233; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    receive_mode <= 1; data_in <= 32'h44556677; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    receive_mode <= 1; data_in <= 32'h8899AABB; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    receive_mode <= 1; data_in <= 32'hCCDDEEFF; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    repeat(4) @(posedge clk);

    rst <= 1; @(posedge clk);
    rst <= 0; @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'h00112233); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 1; #1 assert32(data_out, 32'h00112233); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'h44556677); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 1; #1 assert32(data_out, 32'h44556677); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 1; #1 assert32(data_out, 32'h8899AABB); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'hCCDDEEFF); @(posedge clk);

    receive_mode <= 0; data_in <= 32'h00000000; action <= 1; #1 assert32(data_out, 32'hCCDDEEFF); @(posedge clk);
    receive_mode <= 0; data_in <= 32'h00000000; action <= 0; #1 assert32(data_out, 32'hxxxxxxxx); @(posedge clk);
    repeat(10) @(posedge clk); $finish;
end

initial begin
    $dumpfile("buffer_tb.vcd");
    $dumpvars(-1, buffer_tb);
end

endmodule
