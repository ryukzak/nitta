module pu_simple_control #
        ( parameter MICROCODE_WIDTH = 16
        , parameter MEMORY_SIZE = 200
        , parameter PROGRAM_DUMP = "dump.list"
        , parameter PROGRAM_COUNTER_WIDTH = $clog2( MEMORY_SIZE )
        )
    ( input wire                        clk
    , input wire                        rst
    , input wire                        signal_cycle_start

    , output wire                       flag_cycle_begin
    , output wire                       flag_in_cycle
    , output wire                       flag_cycle_end

    , output wire [MICROCODE_WIDTH-1:0] signals_out
    );

reg [MICROCODE_WIDTH-1:0]       program_memory[MEMORY_SIZE-1:0];
reg [PROGRAM_COUNTER_WIDTH-1:0] pc;

initial $readmemh(PROGRAM_DUMP, program_memory, 0, MEMORY_SIZE-1);

always @(posedge clk)
    if      ( rst )                                          pc <= 0;
    else if ( pc == 0 && signal_cycle_start )                pc <= 1;

    else if ( pc >= MEMORY_SIZE - 1 && !signal_cycle_start ) pc <= 0;
    else if ( pc >= MEMORY_SIZE - 1 &&  signal_cycle_start ) pc <= 1;

    else if ( pc > 0 )                                       pc <= pc + 1;
    // if pc == 0 && !start_cycle - nothing to do


assign signals_out = rst ? program_memory[0] : program_memory[pc];

assign flag_cycle_begin = pc == 1;
assign flag_in_cycle    = pc != 0;
assign flag_cycle_end   = pc == MEMORY_SIZE - 1;

endmodule
