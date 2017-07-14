module dpu_fram_tb();

parameter W = 32;
parameter WA = 4;

reg clk, wr, oe;
reg [3:0] addr; 

reg [W-1:0]  value_i;
reg [WA-1:0] attr_i;

wire [W-1:0]  value_o;
wire [WA-1:0] attr_o;

dpu_fram u(
	.dp_clk(clk),
	
	.dp_addr(addr),

	.dp_wr(wr),	
	.dp_data(value_i),
	.dp_attr_i(attr_i),

	.dp_oe(oe),
	.dp_value(value_o),
	.dp_attr_o(attr_o)
	);

initial
	begin
	   $dumpfile("test.vcd");
     $dumpvars(0, dpu_fram_tb);
     clk <= 0; addr <= 0; wr <= 0; oe <= 0; value_i <= 0;
     u.bank[0] <= 8'hF0;
     u.bank[1] <= 8'hF1;
     u.bank[2] <= 8'hF2;

     `include "dpu_fram_tb_process.v"

     // #2 addr <= 0; wr <= 1; oe <= 0; value_i <= 8'hAA; // save 0 AA
     
     // #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; value_i <= 0; // nop
     // #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; value_i <= 0; // nop

     // #2 addr <= 0; wr <= 0; oe <= 1; value_i <= 8'h00; // load 0 AA

     // #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; value_i <= 0; // nop
     // #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; value_i <= 0; // nop
   
     #20 $finish;
	end

always #1 clk = !clk;

endmodule
