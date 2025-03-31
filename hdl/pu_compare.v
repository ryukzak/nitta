module pu_compare
    #( parameter DATA_WIDTH = 32
    , parameter ATTR_WIDTH = 4
    )
    ( input  wire                  clk
    , input  wire                  rst
    
    // Control signals
    , input  wire                  signal_wr
    , input  wire [DATA_WIDTH-1:0] data_in
    , input  wire [ATTR_WIDTH-1:0] attr_in
    
    // Output
    , input  wire                  signal_oe
    , output wire [DATA_WIDTH-1:0] data_out
    , output wire [ATTR_WIDTH-1:0] attr_out
    );

// Internal storage
reg [DATA_WIDTH-1:0] arg [0:1];
reg arg_sel;
reg operation;

// Operation codes
localparam 
    CMP_EQ  = 3'b000,
    CMP_LT  = 3'b001,
    CMP_LTE = 3'b010,
    CMP_GT  = 3'b011,
    CMP_GTE = 3'b100;

// Input processing
always @(posedge clk) begin
    if (rst) begin
        arg[0] <= 0;
        arg[1] <= 0;
        arg_sel <= 0;
    end else if (signal_wr) begin
        arg[arg_sel] <= data_in;
        operation <= attr_in[2:0]; // Use first 3 bits for operation
        arg_sel <= !arg_sel;
    end
end

// Comparison logic
reg [DATA_WIDTH-1:0] result;
always @(posedge clk) begin
    if (arg_sel) begin // Both operands received
        case(operation)
            CMP_EQ:  result <= (arg[0] == arg[1]);
            CMP_LT:  result <= (arg[0] < arg[1]);
            CMP_LTE: result <= (arg[0] <= arg[1]);
            CMP_GT:  result <= (arg[0] > arg[1]);
            CMP_GTE: result <= (arg[0] >= arg[1]);
            default: result <= 0;
        endcase
    end
end

// Output control
assign data_out = signal_oe ? result : {DATA_WIDTH{1'bz}};
assign attr_out = { {ATTR_WIDTH-1{1'b0}}, signal_oe };

endmodule
