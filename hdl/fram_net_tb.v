module fram_net_tb();

   parameter W = 32;
   parameter WA = 4;
   parameter WS = 16;
   
   reg clk, rst;
   reg [3:0] addr; 
   

   wire [W-1:0] dp_data;
   wire [WA-1:0] dp_attr;

   wire [W-1:0] fram1_data_i;
   wire [WA-1:0] fram1_attr_i;
   wire [W-1:0] fram2_data_i;
   wire [WA-1:0] fram2_attr_i;

   wire [W-1:0] value_o;
   wire [WA-1:0] attr_o;
   reg  [32:0]        i;
   
   reg [WS-1:0]        wires;

   dpu_fram fram1(.dp_clk(clk),
	                .dp_oe(wires[15]),
	                .dp_wr(wires[14]),	
	                .dp_addr(wires[11:8]),
	                .dp_data(dp_data),
	                .dp_attr_i(dp_attr),
	                .dp_value(fram1_data_i),
	                .dp_attr_o(fram1_attr_i)
	                );

   dpu_fram fram2(.dp_clk(clk),
	                .dp_oe(wires[7]),
	                .dp_wr(wires[6]),	
	                .dp_addr(wires[3:0]),
	                .dp_data(dp_data),
	                .dp_attr_i(dp_attr),
	                .dp_value(fram2_data_i),
	                .dp_attr_o(fram2_attr_i)
	                );

   assign { dp_data, dp_attr } = { fram1_data_i, fram1_attr_i } |
                                 { fram2_data_i, fram2_attr_i } ;

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
        $dumpvars(0, fram_net_tb);

        for ( i = 0; i < 10; i = i + 1) begin
           fram1.bank[i] <= 8'hA0 + i + 1;
           fram2.bank[i] <= 8'hB0 + i + 1;
        end  
        wires <= 0; clk <= 0;

        @(negedge rst);

`include "fram_net_tb_process.v"

        repeat(4) @(posedge clk); $finish;

	   end

endmodule
