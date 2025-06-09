module pu_multiplier
    #( parameter DATA_WIDTH           = 32
    , parameter ATTR_WIDTH           = 4
    , parameter SCALING_FACTOR_POWER = 0
    , parameter INVALID              = 0
    , parameter FLOAT                = 1
    )
    ( input  wire                  clk
    , input  wire                  rst

    , input  wire                  signal_wr
    , input  wire [DATA_WIDTH-1:0] data_in
    , input  wire [ATTR_WIDTH-1:0] attr_in

    , input  wire                  signal_oe
    , output wire [DATA_WIDTH-1:0] data_out
    , output wire [ATTR_WIDTH-1:0] attr_out
    );

reg [DATA_WIDTH-1:0]              arg [0:1];
reg                               arg_invalid [0:1];
reg                               arg_sel;

always @(posedge clk) begin
    if ( rst ) begin
        arg[0] <= 0;
        arg[1] <= 0;
        arg_sel <= 0;
    end else begin
        if ( signal_wr ) begin
            arg[arg_sel] <= data_in[DATA_WIDTH-1:0];
            arg_invalid[arg_sel] <= attr_in[INVALID];
            arg_sel <= !arg_sel;
        end
    end
end


wire signed [DATA_WIDTH-1:0]         mult_result;

generate
    if ( FLOAT ) begin
        wire [DATA_WIDTH-1:0] a = arg[0];
        wire [DATA_WIDTH-1:0] b = arg[1];
        wire [DATA_WIDTH-1:0] mult_result_float;

        float_mult #(
            .DATA_WIDTH(DATA_WIDTH)
        ) mult_i2 (
            .dataa(a),
            .datab(b),
            .result(mult_result_float)
        );
        assign mult_result = mult_result_float;
    end else begin
        wire signed [DATA_WIDTH/2-1:0] dataa = arg[0][DATA_WIDTH/2-1:0];
        wire signed [DATA_WIDTH/2-1:0] datab = arg[1][DATA_WIDTH/2-1:0];
        wire signed [DATA_WIDTH-1:0] mult_result_fixed;

        mult_inner #(
            .DATA_WIDTH(DATA_WIDTH)
        ) mult_i1 (
            .dataa(dataa),
            .datab(datab),
            .result(mult_result_fixed)
        );
        assign mult_result = mult_result_fixed;
    end
endgenerate

reg f;
reg write_multresult;
reg invalid_value;

wire [DATA_WIDTH/2-1:0] arg1_high_part, arg2_high_part, zero;
assign zero = {DATA_WIDTH/2{1'b0}};
assign arg1_high_part = arg[0][DATA_WIDTH-1:DATA_WIDTH/2-1];
assign arg2_high_part = arg[1][DATA_WIDTH-1:DATA_WIDTH/2-1];


always @(posedge clk) begin
    if ( FLOAT ) begin
        invalid_value <= arg_invalid[0] || arg_invalid[1];
    end else begin
        invalid_value <= arg_invalid[0] || arg_invalid[1]
                    || !( arg1_high_part == zero ^ ~arg1_high_part == zero )
                    || !( arg2_high_part == zero ^ ~arg2_high_part == zero );
    end
    if ( rst ) begin
        f <= 0;
        write_multresult <= 0;
    end else begin
        f <= arg_sel; // actual register will be updateted only on next clk
        if ( arg_sel == 0 && f == 1 ) write_multresult <= 1;
        else                          write_multresult <= 0;
    end
end

reg                  invalid_result;
reg [DATA_WIDTH-1:0] data_multresult;

always @(posedge clk) begin
    if ( rst ) begin
        data_multresult <= 0;
        invalid_result <= 0;                                                           
    end else begin
        if ( write_multresult ) begin
            invalid_result <= invalid_value;
            if ( FLOAT ) begin
                data_multresult <= mult_result;
            end else begin
                data_multresult <= mult_result >>> SCALING_FACTOR_POWER;
            end
        end
    end
end

assign data_out = signal_oe ? data_multresult : 0;
assign attr_out = { {ATTR_WIDTH-1{1'b0}}, signal_oe ? invalid_result : 1'b0 };

endmodule
