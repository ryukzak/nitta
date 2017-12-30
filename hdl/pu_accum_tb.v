`timescale 1 ps/ 1 ps
module pu_accum_tb
#(   parameter DATA_WIDTH = 4
  ,  parameter ATTR_WIDTH = 4
  ,  parameter SIGN       = 0
  ,  parameter OVERFLOW   = 1
)
(); 
reg clk 
,   signal_load
,   signal_init
,   signal_neg
,   signal_oe; 

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in = 0;

wire [DATA_WIDTH-1:0] data_out;

pu_accum unit_under_test (
    .clk(clk),
    .signal_load(signal_load),
    .signal_init(signal_init),
    .signal_neg(signal_neg),
    .signal_oe(signal_oe),
    .data_in(data_in),
    .attr_in(attr_in),
    .data_out(data_out)
);


task nop; // nop
    begin
        signal_load <= 0;
        signal_init <= 0;
        signal_neg  <= 0;
        signal_oe   <= 0;
        data_in     <= 0;
    end
endtask

task Initialization;
    input [DATA_WIDTH-1:0] id;
    begin
        signal_init <= id;        
    end
endtask

task Load;
    input [DATA_WIDTH-1:0] id;
    begin
        signal_load <= id;
        signal_init = 0;
        signal_oe   = 0;
        signal_neg  = 0;
    end
endtask

task outputdata;
    input sig;
    begin
        signal_load <= 0;
        signal_init <= 0;
        signal_neg  <= 0;
        signal_oe   <= sig;
    end
endtask

task Send_value;
    input [DATA_WIDTH-1:0] data;
    begin
        data_in = data;
    end
endtask

task Send_attr_in;
    input [ATTR_WIDTH-1:0] attr;
    begin
        attr_in = attr;
    end
endtask

task Send_neg;
    input x_neg;
    begin
        signal_neg = x_neg;
    end
endtask

always
  #5 clk = ~clk;

reg RST;

initial 
  begin
    RST <= 1;
    $display("Start programm");
    clk = 0;

    nop(); @(posedge clk);
 
    RST <= 0;
    ///////////////////////////////////////////////////////////////
    nop(); @(posedge clk);
    ///////////////////////////////////////////////////////////////    
    // Load(1);            @(posedge clk);
    // Initialization(1);  repeat (2) @(posedge clk);
    // Initialization(0);  repeat (2) @(posedge clk);
    // Send_value(2);      repeat(2)   @(posedge clk); // сложит сам с собой из-за задержки
    // Send_neg(0);        @(posedge clk);
    // Send_value(3);      @(posedge clk);
    // Load(0);            @(posedge clk);
    // outputdata(0);      @(posedge clk);
    ///////////////////////////////////////////////////////////////
    // Load(1);            @(posedge clk);
    // Initialization(1);  repeat (2) @(posedge clk);
    // Initialization(0);  repeat (2) @(posedge clk);
    // Send_value(7);      @(posedge clk);
    // Send_neg(0);        @(posedge clk);
    // Send_value(8);      @(posedge clk);
    // Load(0);            @(posedge clk);
    // outputdata(1);      @(posedge clk);
    ////////////////////////////////////////////////////////////////
    // Load(1);                    @(posedge clk);
    // Initialization(1);          repeat (2) @(posedge clk);
    // Initialization(0);          repeat (2) @(posedge clk);
    // Load(0);                    @(posedge clk);
    // Load(1);
    // Send_value(9);              @(posedge clk);
    // Load(0);                    @(posedge clk);
    // //Send_Neg(0);              @(posedge clk); // start
    // Load(1);
    // Send_value(3);              @(posedge clk);    
    // Load(0);                    @(posedge clk);
    // //Send_Neg(0);              @(posedge clk);
    // outputdata(1);              @(posedge clk);
    // outputdata(0);              @(posedge clk);
    // repeat(10) @(posedge clk);
    ///////////////////////////////////////////////////////////////
    nop();                            repeat(10) @(posedge clk);
    outputdata(0);                    repeat(10) @(posedge clk);
    ///////////////////////////////////////////////////////////////
    // Load_Data(1);                @(posedge clk);
    // Initialization (1);          repeat (2) @(posedge clk);
    // Initialization (0);          repeat (2) @(posedge clk);
    // Send_Value(5);               repeat(2) @(posedge clk);
    // Send_Data_Out(1);            @(posedge clk);
    // Send_Data_Out(0);            @(posedge clk);
    // Send_Value(1);               repeat(2) @(posedge clk);
    // Send_Value(data_out);        @(posedge clk);
    // Send_Data_Out(1);            @(posedge clk);
    // Load_Data(0);                @(posedge clk);
    // Send_Data_Out(0);            @(posedge clk);
    $finish;
  end  

  initial
  begin
    $dumpfile("pu_accum.vcd");
    $dumpvars(0,pu_accum_tb);
    $display("finish");
  end 
       
endmodule

// ----------------------------------------------
// iverilog -o test -I./ -y./ pu_accum.v pu_accum_tb.v
// vvp test
// gtkwave bench.vcd
// iverilog -o test -I./ -y./ bench.v testbench.v && vvp test && gtkwave bench.vcd
// ----------------------------------------------