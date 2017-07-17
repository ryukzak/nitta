module dpu_fram_tb();

   parameter W = 32;
   parameter WA = 4;

   reg clk, rst, wr, oe;
   reg [3:0] addr; 

   reg [W-1:0] value_i;
   reg [WA-1:0] attr_i;

   wire [W-1:0] value_o;
   wire [WA-1:0] attr_o;

   reg [32:0]    i;
   reg [32:0]    ctrl;
   reg           x;
   

   dpu_fram fram(.dp_clk(clk),
	               .dp_addr(addr),

	               .dp_wr(wr),	
	               .dp_data(value_i),
	               .dp_attr_i(attr_i),

	               .dp_oe(oe),
	               .dp_value(value_o),
	               .dp_attr_o(attr_o)
	               );

   initial begin
      clk = 1'b0;
      rst = 1'b1;
      repeat(4) #10 clk = ~clk;
      rst = 1'b0;
      forever #10 clk = ~clk;
   end

   initial
	   begin
	      $dumpfile("test.vcd");
        $dumpvars(0, dpu_fram_tb);
        addr <= 0; wr <= 0; oe <= 0; value_i <= 0;

        for ( i = 0; i < 10; i = i + 1) begin
           fram.bank[i] <= 8'hA0 + i;
        end  

        @(negedge rst);
        
`include "dpu_fram_tb.control.v"
        
        repeat(4) @(posedge clk); $finish;

	   end

   initial
	   begin
        @(negedge rst);     

`include "dpu_fram_tb.asserts.v"

        repeat(4) @(posedge clk); $finish;

	   end
   
endmodule
