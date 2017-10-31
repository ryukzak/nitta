module pu_mux_tb();

parameter W = 32;
parameter WA = 4;

reg clk;
reg load, sel;
reg [1:0] mode;
reg [W-1:0]  data_in;
reg [WA-1:0] attr_in;

reg oe;
wire [W-1:0]  data_out;
wire [WA-1:0] attr_out;

pu_mux pu_mux(
  .clk(clk),
    
  .signal_load(load), .signal_sel(sel), .signal_mode(mode),
  .data_in(data_in), .attr_in(attr_in),

  .signal_oe(oe),    
  .data_out(data_out), .attr_out(attr_out)
  );

initial begin
  clk = 0;
  forever #10 clk = !clk;
end

task nop;
begin
  load <= 0;
  sel <= 0;
  mode <= 0;
  oe <= 0;
end
endtask


initial
  begin
    $dumpfile("pu_mux_tb.vcd");
    $dumpvars(0, pu_mux_tb);
    nop(); repeat (2) @(posedge clk);
    data_in <= 2 << (W - 1 - 2); load <= 1; sel <= 1; mode <= 3; @(posedge clk);
    nop(); @(posedge clk);
    data_in <= 'hA1; load <= 1; sel <= 0; @(posedge clk);
    data_in <= 'hA2; load <= 1; sel <= 0; @(posedge clk);
    nop(); @(posedge clk);
    nop(); @(posedge clk);
    data_in <= 'hA3; load <= 1; sel <= 0; @(posedge clk);
    data_in <= 'hA4; load <= 1; sel <= 0; @(posedge clk);
    nop(); @(posedge clk);
    oe <= 1; @(posedge clk);
    repeat (4) @(posedge clk);
    $finish;
  end


endmodule
