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
    CMP_EQ  = 3'b000,
    CMP_LT  = 3'b001,
    CMP_LTE = 3'b010,
    CMP_GT  = 3'b011,
    CMP_GTE = 3'b100;

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
        operation <= op_sel;
        arg_sel <= arg_sel + 1;
    end
end

reg [DATA_WIDTH-1:0] result;
always @(posedge clk) begin
    if (arg_sel == 2) begin
        case(operation)
            CMP_EQ: result <= (arg[0] == arg[1]);
            CMP_LT: result <= (arg[0] < arg[1]);
            CMP_LTE: result <= (arg[0] <= arg[1]);
            CMP_GT: result <= (arg[0] > arg[1]);
            CMP_GTE: result <= (arg[0] >= arg[1]);
            default: result <= 0;
        endcase
        arg_sel <= 0;
    end
end
assign attr_out = attr_in;
assign data_out = oe ? result : 0;

always @(posedge clk) begin
    if (oe) begin
        $display("-------------------------");
        $display("operation = %0d", operation);
        $display("arg[0] = %0d", arg[0]);
        $display("arg[1] = %0d", arg[1]);
        $display("data_out = %0d", data_out);
        $display("-------------------------");
    end
end

endmodule
