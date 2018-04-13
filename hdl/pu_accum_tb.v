`timescale 1 ps/ 1 ps
module pu_accum_tb
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter SIGN       = 0
   , parameter OVERFLOW   = 1
   )
  (); 

reg clk, rst, signal_load, signal_init, signal_neg, signal_oe;

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;

wire [DATA_WIDTH-1:0] data_out;

pu_accum 
  #( .DATA_WIDTH( DATA_WIDTH )
   , .ATTR_WIDTH( ATTR_WIDTH )
   ) unit_under_test 
  ( .clk(clk)
  , .rst(rst)
  , .signal_init(signal_init)
  , .signal_load(signal_load)
  , .signal_neg(signal_neg)
  , .signal_oe(signal_oe)
  , .data_in(data_in)
  , .attr_in(attr_in)
  , .data_out(data_out)
  );

task nop;
  begin
    signal_init <= 0;
    signal_load <= 0;
    signal_neg  <= 0;
    signal_oe   <= 0;
    data_in     <= 0;
    attr_in     <= 0;
  end
endtask

task init;
  input neg;
  input [DATA_WIDTH-1:0] arg;
  begin
    signal_init <= 1;
    signal_load <= 1;
    signal_neg  <= neg;
    signal_oe   <= 0;
    data_in     <= arg;
    attr_in     <= 0;
  end
endtask

task load;
  input neg;
  input [DATA_WIDTH-1:0] arg;
  begin
    signal_init <= 0;
    signal_load <= 1;
    signal_neg  <= neg;
    signal_oe   <= 0;
    data_in     <= arg;
    attr_in     <= 0;
  end
endtask

task out;
  begin
    signal_init <= 0;
    signal_load <= 0;
    signal_neg  <= 0;
    signal_oe   <= 1;
    data_in     <= 0;
    attr_in     <= 0;
  end
endtask




initial begin
  $dumpfile("pu_accum_tb.vcd");
  $dumpvars(0, pu_accum_tb);
  clk <= 1;
  forever #1 clk <= !clk;
end

initial begin
  rst <= 1;     @(posedge clk);
  @(posedge clk);
  @(posedge clk);
  rst <= 0;
end

initial begin
  nop();
  @(negedge rst);

  nop(); @(posedge clk);
  init(0, 5); @(posedge clk);
  load(0, 5); @(posedge clk);
  nop(); @(posedge clk);
  out(); @(posedge clk);
  
  @(posedge clk);
  
  $finish;
end  
       
endmodule
