module float_mult
    #( parameter DATA_WIDTH = 32 )
    ( input [DATA_WIDTH-1:0] dataa,
      input [DATA_WIDTH-1:0] datab,
      output reg [DATA_WIDTH-1:0] result
    );

    wire        sign_a = dataa[31];
    wire [7:0]  exp_a  = dataa[30:23];
    wire [23:0] mant_a = {1'b1, dataa[22:0]};

    wire        sign_b = datab[31];
    wire [7:0]  exp_b  = datab[30:23];
    wire [23:0] mant_b = {1'b1, datab[22:0]};

    wire        res_sign;
    wire [7:0]  res_exp;
    wire [47:0] product;
    wire [22:0] res_mant;

    wire a_zero = (exp_a == 0) && (mant_a[22:0] == 0);
    wire b_zero = (exp_b == 0) && (mant_b[22:0] == 0);
    wire a_inf  = (exp_a == 8'hFF) && (mant_a[22:0] == 0);
    wire b_inf  = (exp_b == 8'hFF) && (mant_b[22:0] == 0);
    wire a_nan  = (exp_a == 8'hFF) && (mant_a[22:0] != 0);
    wire b_nan  = (exp_b == 8'hFF) && (mant_b[22:0] != 0);

    assign res_sign = sign_a ^ sign_b;

    assign product = mant_a * mant_b;

    wire [8:0] exp_sum = exp_a + exp_b - 127;
    wire [8:0] norm_exp = product[47] ? exp_sum + 1 : exp_sum;
    
    assign res_mant = product[47] ? (product[46:24] + product[23]) : (product[45:23] + product[22]);

    always @(*) begin
        if (a_nan || b_nan) begin
            result = {res_sign, 8'hFF, 23'h1};
        end else if ((a_inf && b_zero) || (b_inf && a_zero)) begin
            result = {res_sign, 8'hFF, 23'h1};
        end else if (a_inf || b_inf) begin
            result = {res_sign, 8'hFF, 23'h0};
        end else if (a_zero || b_zero) begin
            result = {res_sign, 8'h0, 23'h0};
        end else if (norm_exp[8] || (norm_exp[7:0] > 254)) begin
            result = {res_sign, 8'hFF, 23'h0};
        end else if (norm_exp[7:0] < 1) begin
            result = {res_sign, 8'h0, 23'h0};
        end else begin
            result = {res_sign, norm_exp[7:0], res_mant};
        end
    end

endmodule