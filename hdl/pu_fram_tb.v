module dpu_fram_tb();

parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;

reg clk, wr, oe;
reg [3:0] addr; 

reg [DATA_WIDTH-1:0]  data_in;
reg [ATTR_WIDTH-1:0]  attr_in;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;

pu_fram 
	#( .RAM_SIZE( 16 )
	 , .DATA_WIDTH( 32 )
	 , .ATTR_WIDTH( 4 )
	) u (
	.clk(clk),
	
	.signal_addr(addr),

	.signal_wr(wr),	
	.data_in(data_in),
	.attr_in(attr_in),

	.signal_oe(oe),
	.data_out(data_out),
	.attr_out(attr_out)
	);

initial
	begin
	   $dumpfile("test.vcd");
     $dumpvars(0, dpu_fram_tb);
     clk <= 0; addr <= 0; wr <= 0; oe <= 0; data_in <= 0;

     #2 addr <= 0; wr <= 1; oe <= 0; data_in <= 8'hAA; // save 0 AA
     
     #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; data_in <= 0; // nop
     #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; data_in <= 0; // nop

     #2 addr <= 0; wr <= 0; oe <= 1; data_in <= 8'h00; // load 0 AA

     #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; data_in <= 0; // nop
     #2 clk <= 0; addr <= 0; wr <= 0; oe <= 0; data_in <= 0; // nop
   
     #20 $finish;
	end

always #1 clk = !clk;

endmodule
