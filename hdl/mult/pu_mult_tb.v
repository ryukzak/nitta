
// Copyright (C) 2017  Intel Corporation. All rights reserved.
// Your use of Intel Corporation's design tools, logic functions 
// and other software and tools, and its AMPP partner logic 
// functions, and any output files from any of the foregoing 
// (including device programming or simulation files), and any 
// associated documentation or information are expressly subject 
// to the terms and conditions of the Intel Program License 
// Subscription Agreement, the Intel Quartus Prime License Agreement,
// the Intel FPGA IP License Agreement, or other applicable license
// agreement, including, without limitation, that your use is for
// the sole purpose of programming logic devices manufactured by
// Intel and sold by Intel or its authorized distributors.  Please
// refer to the applicable agreement for further details.

// *****************************************************************************
// This file contains a Verilog test bench template that is freely editable to  
// suit user's needs .Comments are provided in each section to help the user    
// fill out necessary details.                                                  
// *****************************************************************************
// Generated on "01/13/2018 17:35:53"
                                                                                
// Verilog Test Bench template for design : play
// 
// Simulation tool : ModelSim-Altera (Verilog)
// 

`timescale 1 ps/ 1 ps

module pu_mult_tb
#(parameter DATA_WIDTH = 32,
  parameter ATTR_WIDTH =  4,
  parameter INVALID    =  0
  );
  
reg  [DATA_WIDTH-1:0]  data_in;
reg                  signal_oe;
reg                        rst;
reg                 signal_sel;
reg                  signal_wr;
reg                        clk;
reg  [ATTR_WIDTH:0]    attr_in;                                              
wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH:0]   attr_out;

                          
pu_mult 
  #( .DATA_WIDTH( DATA_WIDTH )
   , .ATTR_WIDTH( ATTR_WIDTH )
   , .INVALID( INVALID )
   ) i1 (
// port map - connection between master ports and signals/registers   
    .data_in(data_in),
    .signal_oe(signal_oe),
    .rst(rst),
    .signal_wr(signal_wr),
    .clk(clk),
    .signal_sel(signal_sel),
    .data_out(data_out),
    .attr_in(attr_in),
    .attr_out(attr_out)
);

initial begin
  clk = 0;
  forever #10 clk = !clk;
end

initial begin
  $display("Start programm");

  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0;  
  rst <= 1; repeat (2) @(posedge clk);
  rst <= 0; @(posedge clk);
  
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= 5; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= 32'h00100007; attr_in <= 0; @(posedge clk);

  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat(2) @(posedge clk);

  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat (5) @(posedge clk);
  
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= 4; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= 5; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat(2) @(posedge clk);
  
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat (5) @(posedge clk);
  
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= -10; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= -10; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat(2) @(posedge clk);
  
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat (5) @(posedge clk);
   
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= 32'h00000111; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= 32'h00000111; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat(2) @(posedge clk);
  
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat (5) @(posedge clk);
  
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= 4; attr_in <= 1; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= 5; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat(2) @(posedge clk);
  
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat (5) @(posedge clk);
  
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 0; data_in <= -4; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 1; signal_sel <= 1; data_in <= 5; attr_in <= 0; @(posedge clk);
  signal_oe <= 0; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat(2) @(posedge clk);
  
  signal_oe <= 1; signal_wr <= 0; signal_sel <= 0; data_in <= 0; attr_in <= 0; repeat (5) @(posedge clk);
  
  repeat (20) @(posedge clk);
  $finish();    
end

initial begin
  $dumpfile("mul.vcd");
  $dumpvars(0, pu_mult_tb);
end

endmodule

