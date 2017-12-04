module pu_shift_tb();

parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;

reg clk;
reg signal_work, signal_direction, signal_mode, signal_step, signal_init, signal_oe;
reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;

wire [DATA_WIDTH-1:0] data_out;
wire [ATTR_WIDTH-1:0] attr_out;

pu_shift #( .DATA_WIDTH(DATA_WIDTH)
          , .ATTR_WIDTH(ATTR_WIDTH)
          ) dup (
  .clk(clk)
  , .signal_work(signal_work), .signal_direction(signal_direction)
  , .signal_mode(signal_mode), .signal_step(signal_step)
  , .signal_init(signal_init), .signal_oe(signal_oe)

  , .data_in(data_in), .attr_in(attr_in)
  , .data_out(data_out), .attr_out(attr_out)
  );

initial begin
  clk = 0;
  forever #10 clk = !clk;
end

task nop;
begin
  signal_work <= 0; signal_direction <= 0;
  signal_mode <= 0; signal_step <= 0;
  signal_init <= 0; signal_oe <= 0;
end
endtask

task arithmetic_left1;
begin
  signal_work <= 1; signal_direction <= 1;
  signal_mode <= 1; signal_step <= 0;
  signal_init <= 0; signal_oe <= 0;
end
endtask

task arithmetic_right1;
begin
  signal_work <= 1; signal_direction <= 0;
  signal_mode <= 1; signal_step <= 0;
  signal_init <= 0; signal_oe <= 0;
end
endtask

task init;
  input [DATA_WIDTH-1:0] in;
begin
  signal_work <= 0; signal_direction <= 0;
  signal_mode <= 0; signal_step <= 0;
  signal_init <= 1; signal_oe <= 0;
  data_in <= in;
end
endtask

task oe;
begin
  signal_work <= 0; signal_direction <= 0;
  signal_mode <= 0; signal_step <= 0;
  signal_init <= 0; signal_oe <= 1;
end
endtask

initial
  begin
    $dumpfile("pu_shift_tb.vcd");
    $dumpvars(0, pu_shift_tb);
    nop(); @(posedge clk);
    init('h10); @(posedge clk);
    arithmetic_left1(); @(posedge clk);
    oe(); @(posedge clk);
    arithmetic_right1(); @(posedge clk);
    arithmetic_right1(); @(posedge clk);
    arithmetic_right1(); @(posedge clk);
    oe(); @(posedge clk);
    nop(); @(posedge clk);
    nop(); @(posedge clk);
    nop(); @(posedge clk);
    init('hFFFFFFF0); @(posedge clk);
    arithmetic_right1(); @(posedge clk);
    oe(); @(posedge clk);
    nop(); @(posedge clk);
    $finish;
  end

endmodule
