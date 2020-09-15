module pu_control_tb();

parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;
parameter PROGRAM_SIZE = 200;
parameter INSTRUCTION_SIZE = 16;
parameter PROGRAN_COUNTER_SIZE = $clog2(PROGRAM_SIZE);

reg clk, rst, jump;
reg [3:0] addr;

reg [DATA_WIDTH-1:0] data_in;
reg [ATTR_WIDTH-1:0] attr_in;

wire [INSTRUCTION_SIZE-1:0] control_bus;


pu_control control( .pu_clk(clk),

                    .pu_jump(jump),
                    .pu_data_in(data_in),
                    .pu_attr_in(attr_in),

                    .pu_control_bus(control_bus)
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
     $dumpfile("pu_control_tb.vcd");
     $dumpvars(0, pu_control_tb);

     @(negedge rst);
     jump <= 0; data_in <= 0; attr_in <= 0; @(negedge clk);
     jump <= 0; data_in <= 0; attr_in <= 0; @(negedge clk);
     jump <= 0; data_in <= 0; attr_in <= 0; @(negedge clk);
     jump <= 0; data_in <= 0; attr_in <= 0; @(negedge clk);
     jump <= 0; data_in <= 0; attr_in <= 0; @(negedge clk);
     jump <= 1; data_in <= 0; attr_in <= 0; @(negedge clk);
     jump <= 0; data_in <= 0; attr_in <= 0; @(negedge clk);
     jump <= 0; data_in <= 0; attr_in <= 0; @(negedge clk);


     repeat(4) @(posedge clk); $finish;

  end

endmodule
