module pu_simple_control
  #( parameter MICROCODE_WIDTH = 16
  ,  parameter MEMORY_SIZE = 200
  ,  parameter PROGRAM_DUMP = "dump.list"
  ,  parameter PROGRAM_COUNTER_WIDTH = $clog2(MEMORY_SIZE)
  )
  ( input wire clk
  , input wire rst
  , output wire [MICROCODE_WIDTH-1:0] signals_out
  , output wire cycle
  );


reg [MICROCODE_WIDTH-1:0]       program_memory[MEMORY_SIZE-1:0];
reg [PROGRAM_COUNTER_WIDTH-1:0] program_counter;

   
initial
  begin
    $readmemh(PROGRAM_DUMP, program_memory, 0, MEMORY_SIZE-1);
  end


assign signals_out = rst ? program_memory[0] : program_memory[program_counter];


always @(posedge clk)
  if ( rst || program_counter >= MEMORY_SIZE - 1 )
    program_counter <= 1;
  else
    program_counter <= program_counter + 1;

assign cycle = program_counter == 1;

endmodule
