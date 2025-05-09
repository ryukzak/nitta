module pu_div
    #( parameter DATA_WIDTH          = 32
    , parameter ATTR_WIDTH           = 4
    , parameter INVALID              = 0
    , parameter PIPELINE             = 4
    , parameter SCALING_FACTOR_POWER = 0
    , parameter MOCK_DIV             = 0
    )
    ( input  wire                  clk
    , input  wire                  rst

    , input  wire                  signal_sel

    , input  wire                  signal_wr
    , input  wire [DATA_WIDTH-1:0] data_in
    , input  wire [ATTR_WIDTH-1:0] attr_in

    , input  wire                  signal_oe
    , output wire [DATA_WIDTH-1:0] data_out
    , output wire [ATTR_WIDTH-1:0] attr_out
    );

reg [DATA_WIDTH-1:0]             denom, numer;
reg [ATTR_WIDTH-1:0]             denom_attr, numer_attr;

always @(posedge clk) begin
    if (rst) begin
        denom <= 0;
        denom_attr <= 0;
        numer <= 0;
        numer_attr <= 0;
    end else if (!signal_oe) begin
        if (signal_wr && !signal_sel) begin
            denom <= data_in;
            denom_attr <= attr_in;
        end else if (signal_wr && signal_sel) begin
            numer <= data_in;
            numer_attr <= attr_in;
        end
    end
end

reg [DATA_WIDTH-1:0] denom_latch, numer_latch;
reg denom_invalid_latch, numer_invalid_latch;

always @(posedge clk) begin
    if (rst) begin
        denom_latch <= 0;
        denom_invalid_latch <= 0;
        numer_latch <= 0;
        numer_invalid_latch <= 0;
    end else if (signal_oe && signal_wr) begin
        denom_latch <= denom;
        denom_invalid_latch <= denom_attr[INVALID];
        numer_latch <= numer;
        numer_invalid_latch <= numer_attr[INVALID];
    end
end

wire [DATA_WIDTH/2-1:0] denom_high_part, zero;
assign denom_high_part = denom_latch[DATA_WIDTH-1:DATA_WIDTH/2-1];
assign denom_is_zero = denom_latch == 0;
assign zero = {DATA_WIDTH/2{1'b0}};
assign denom_out_of_range = !(denom_high_part == zero ^ ~denom_high_part == zero);
assign invalid = denom_invalid_latch
                 || numer_invalid_latch
                 || denom_is_zero
                 || denom_out_of_range;

reg [PIPELINE-1:0]   invalid_pipeline;

always @(posedge clk) begin
    if (rst) begin
        invalid_pipeline <= 0;
    end else begin
        invalid_pipeline[0] <= invalid;
        invalid_pipeline[ PIPELINE-1 : 1 ] <= invalid_pipeline[ PIPELINE-2 : 0];
    end
end

wire [DATA_WIDTH-1:0]         quotient_result;
wire [DATA_WIDTH-1:0]         remain_result;

div #
        ( .DATA_WIDTH( DATA_WIDTH )
        , .PIPELINE( PIPELINE )
        , .SCALING_FACTOR_POWER( SCALING_FACTOR_POWER )
        ) div_inner
    ( .numer( numer_latch )
    , .denom( denom_latch )
    , .quotient( quotient_result )
    , .remain( remain_result )
    , .clock( clk )
    );

assign data_out = (signal_oe && !signal_wr)
    ? (signal_sel ? remain_result : quotient_result )//<<< SCALING_FACTOR_POWER)
    : 0;

assign attr_out = {
    {ATTR_WIDTH-1{1'b0}},
    (signal_oe && !signal_wr) ? invalid_pipeline[ PIPELINE - 1 ] : 1'b0
    };

endmodule
