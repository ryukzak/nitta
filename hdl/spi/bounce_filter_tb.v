`timescale 1 ms/ 1 ms
module bounce_filter_tb();

reg  clk;
reg  rst;

always begin
  clk <= 1;
  repeat(4) #5 clk <= ~clk;
  forever #5 clk <= ~clk;
end 


initial begin
  repeat(10) @(posedge clk); $finish;
end

initial begin
  $dumpfile("bounce_filter_tb.vcd");
  $dumpvars(-1, bounce_filter_tb);
end 

endmodule
