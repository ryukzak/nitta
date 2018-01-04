`timescale 1ns/1ps
module pu_buffer_tb
#(  
    parameter DATA_WIDTH  = 8
,   parameter BUF_SIZE    = 6 
)
();

reg clk;
reg ready;
reg rst;

reg  [DATA_WIDTH-1:0] data_in;
wire [DATA_WIDTH-1:0] data_out;

pu_buffer buffer (
  .clk(clk)
, .ready(ready)
, .rst(rst)
, .data_in(data_in)
, .data_out(data_out)
);

always begin
  #5 clk = ~clk;
end 

initial 
  begin
	$display("Start");

	clk     = 0; @(posedge clk); 

	rst		= 1; @(posedge clk); 
	rst		= 0; @(posedge clk);
	
	data_in = 2; 
	ready   = 1; @(posedge clk); @(posedge clk); 
	ready	= 0; @(posedge clk);
	@(posedge clk);@(posedge clk);@(posedge clk);
	data_in = 3; 
	ready   = 1; @(posedge clk); @(posedge clk);
	ready	= 0; @(posedge clk);
	@(posedge clk);@(posedge clk);@(posedge clk);
	data_in = 4; 
	ready   = 1; @(posedge clk); @(posedge clk);
	ready	= 0; @(posedge clk);
	@(posedge clk);@(posedge clk);@(posedge clk);
	data_in = 5; 
	ready   = 1; @(posedge clk); @(posedge clk);
	ready	= 0; @(posedge clk);
	@(posedge clk);@(posedge clk);@(posedge clk);
	data_in = 6; 
	ready   = 1; @(posedge clk); @(posedge clk);
	ready	= 0; @(posedge clk);


	repeat(10) @(posedge clk); $finish;
  end

initial
  begin
	$dumpfile("pu_buffer_tb.vcd");
	$dumpvars(-1, pu_buffer_tb);
	$display("finish");
  end 

endmodule

//  iverilog -o buffer buffer.v buffer_tb.v
//  vvp buffer
//  gtkwave buffer_tb.vcd