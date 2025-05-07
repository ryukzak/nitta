module pu_compare
    #( parameter DATA_WIDTH = 32
    , parameter ATTR_WIDTH = 4
    , parameter SEL_WIDTH = 3
    )
    ( input  wire   clk
    , input  wire   rst

    , input  wire   wr
    , input  wire   oe
    
    , input  wire signed [DATA_WIDTH-1:0] data_in
    , input  wire [ATTR_WIDTH-1:0] attr_in
    , input  wire [SEL_WIDTH-1:0] op_sel
    
    , output [DATA_WIDTH-1:0] data_out
    , output [ATTR_WIDTH-1:0] attr_out
    );

reg signed [DATA_WIDTH-1:0] arg [0:1];
reg [1:0] arg_sel;
reg [2:0] operation;

localparam
    CmpEq  = 3'b000,
    CmpLt  = 3'b001,
    CmpLte = 3'b010,
    CmpGt  = 3'b011,
    CmpGte = 3'b100;

always @(posedge clk)
  if ( rst ) begin
    operation <= 0;
    arg_sel <= 0;
    arg[0] <= 0;
    arg[1] <= 0;
  end

always @(posedge clk) begin
    if (wr) begin
        arg[arg_sel] <= data_in;
        arg_sel <= arg_sel + 1;

        if (arg_sel == 0) begin
            operation <= op_sel;
        end
    end
end

reg [DATA_WIDTH-1:0] result;
always @(posedge clk) begin
    if (arg_sel == 2) begin
        case(operation)
            CmpEq: result <= (arg[0] == arg[1]);
            CmpLt: result <= (arg[0] < arg[1]);
            CmpLte: result <= (arg[0] <= arg[1]);
            CmpGt: result <= (arg[0] > arg[1]);
            CmpGte: result <= (arg[0] >= arg[1]);
            default: result <= 0;
        endcase
        arg_sel <= 0;
    end
end
assign attr_out = attr_in;
assign data_out = oe ? result : 0;

endmodule
