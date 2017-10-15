module pu_accum_tb();

parameter DATA_WIDTH     = 16; 
parameter ATTR_WIDTH     = 2;  

reg clk, load, init, neg, oe;
reg [DATA_WIDTH-1:0]  data_in;
reg [ATTR_WIDTH-1:0]  attr_in;
wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;

pu_accum 
  #( .DATA_WIDTH( DATA_WIDTH )
   , .ATTR_WIDTH ( ATTR_WIDTH )
  ) u (
  .clk(clk),

  .signal_load(load), .signal_init(init), .signal_neg(neg),
  .data_in(data_in), .attr_in(attr_in),

  .signal_oe(oe),
  .data_out(data_out), .attr_out(attr_out)
);

initial
  begin
    $dumpfile("dpu_accum_tb.vcd"); $dumpvars(0, pu_accum_tb); 
    clk <= 1; load <= 0; init <= 0; neg <= 0; oe <= 0; #2
    /*Nop */ load <= 0; init <= 0; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0000; #2

    // /*Init*/ load <= 1; init <= 1; neg <= 1; oe <= 0; attr_in <= 0; data_in <= 'h0002; #2
    // /*Init*/ load <= 1; init <= 1; neg <= 1; oe <= 0; attr_in <= 0; data_in <= 'h0002; #2
    // /*Load*/ load <= 1; init <= 0; neg <= 1; oe <= 0; attr_in <= 0; data_in <= 'h0003; #2
    // /*OE  */ load <= 0; init <= 0; neg <= 0; oe <= 1; attr_in <= 0; data_in <= 'h0000; #2
    // /*Nop */ load <= 0; init <= 0; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0000; #2

    // /*Init*/ load <= 1; init <= 1; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0002; #2
    // /*Init*/ load <= 1; init <= 1; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0002; #2
    // /*Load*/ load <= 1; init <= 0; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0003; #2
    // /*OE  */ load <= 0; init <= 0; neg <= 0; oe <= 1; attr_in <= 0; data_in <= 'h0000; #2
    // /*Nop */ load <= 0; init <= 0; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0000; #2
  
    /*Init*/ load <= 1; init <= 1; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h7fff; #2
    /*Init*/ load <= 1; init <= 1; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h7fff; #2
    /*Load*/ load <= 1; init <= 0; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0001; #2
    /*OE  */ load <= 0; init <= 0; neg <= 0; oe <= 1; attr_in <= 0; data_in <= 'h0000; #2
    /*OE  */ load <= 0; init <= 0; neg <= 0; oe <= 1; attr_in <= 0; data_in <= 'h0000; #2
    /*Nop */ load <= 0; init <= 0; neg <= 0; oe <= 0; attr_in <= 0; data_in <= 'h0000; #2

    #6 $finish;
  end

always #1 clk = !clk;

endmodule
