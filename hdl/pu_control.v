module pu_control(
                   pu_clk,
                   pu_jump,
                   pu_data_in,
                   pu_attr_in,
                   pu_control_bus
                 );

parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;
parameter PROGRAM_SIZE = 200;
parameter INSTRUCTION_SIZE = 16;
parameter PROGRAN_COUNTER_SIZE = $clog2(PROGRAM_SIZE);

input                pu_clk;
input                pu_jump;
input [DATA_WIDTH-1:0]  pu_data_in;
input [ATTR_WIDTH-1:0]  pu_attr_in;
output [INSTRUCTION_SIZE-1:0] pu_control_bus;

reg [INSTRUCTION_SIZE-1:0]     program_memory[PROGRAM_SIZE-1:0];
reg [PROGRAN_COUNTER_SIZE-1:0] program_counter;

input [DATA_WIDTH-1:0]    data_in;
input [ATTR_WIDTH-1:0]    attr_in;
reg [INSTRUCTION_SIZE-1:0] control_bus;


initial
  begin
     $readmemh("dump/control.hex", program_memory, 0, PROGRAM_SIZE-1);
  end


always @(posedge pu_clk)
  control_bus <= program_memory[program_counter];

assign pu_control_bus = control_bus;


always @(posedge pu_clk)
  if (pu_jump)
    program_counter <= pu_data_in;
  else
    if (program_counter < PROGRAM_SIZE)
      program_counter <= program_counter + 1;
    else
      program_counter <= 0;


endmodule
