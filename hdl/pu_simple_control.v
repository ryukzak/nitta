module pu_simple_control #
        ( parameter MICROCODE_WIDTH = 16
        , parameter MEMORY_SIZE = 200
        , parameter PROGRAM_DUMP = "dump.list"
        , parameter PROGRAM_COUNTER_WIDTH = $clog2( MEMORY_SIZE )
        )
    ( input wire                        clk
    , input wire                        rst
    , input wire                        start_cycle

    , output wire                       cycle
    , output wire [MICROCODE_WIDTH-1:0] signals_out

    , output wire [7:0] trace_pc
    );

reg [MICROCODE_WIDTH-1:0]       program_memory[MEMORY_SIZE-1:0];
reg [PROGRAM_COUNTER_WIDTH-1:0] pc;
   
initial $readmemh(PROGRAM_DUMP, program_memory, 0, MEMORY_SIZE-1);

assign cycle = pc == 1;

always @(posedge clk)
    if      ( rst )                                   pc <= 0;
    else if ( pc == 0 && start_cycle )                pc <= 1;

    else if ( pc >= MEMORY_SIZE - 1 && !start_cycle ) pc <= 0;
    else if ( pc >= MEMORY_SIZE - 1 &&  start_cycle ) pc <= 1;

    else if ( pc > 0 )                                pc <= pc + 1;
    // if pc == 0 && !start_cycle - nothing to do


assign signals_out = rst ? program_memory[0] : program_memory[pc];
assign trace_pc = pc;

endmodule
