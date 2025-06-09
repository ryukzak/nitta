`timescale 1 ps / 1 ps

module div
  #( parameter PIPELINE = 4
   , parameter DATA_WIDTH = 32 
   )
  ( input wire clock
  , input wire [DATA_WIDTH-1:0] numer
  , input wire [DATA_WIDTH-1:0] denom
  , output reg [DATA_WIDTH-1:0] quotient
  , output wire [DATA_WIDTH-1:0] remain
  );

reg sign_a, sign_b;
reg [7:0] exp_a, exp_b;
reg [47:0] mant_a;
reg [23:0] mant_b;

always @(posedge clock) begin
  sign_a <= numer[31];
  sign_b <= denom[31];
  exp_a <= numer[30:23];
  exp_b <= denom[30:23];
  mant_a <= {1'b1, numer[22:0], 24'b0};
  mant_b <= {1'b1, denom[22:0]};
end

reg a_zero, b_zero, a_inf, b_inf, a_nan, b_nan;

always @(posedge clock) begin
  a_zero <= (exp_a == 0) && (mant_a[22:0] == 0);
  b_zero <= (exp_b == 0) && (mant_b[22:0] == 0);
  a_inf  <= (exp_a == 8'hFF) && (mant_a[22:0] == 0);
  b_inf  <= (exp_b == 8'hFF) && (mant_b[22:0] == 0);
  a_nan  <= (exp_a == 8'hFF) && (mant_a[22:0] != 0);
  b_nan  <= (exp_b == 8'hFF) && (mant_b[22:0] != 0);
end

reg res_sign;
reg [8:0] res_exp;
reg [24:0] quot;
reg [22:0] res_mant;

always @(posedge clock) begin
  res_sign <= sign_a ^ sign_b;
end

always @(posedge clock) begin
  quot <= mant_a / mant_b;
end

always @(posedge clock) begin
  res_exp <= exp_a - exp_b + 126 + quot[24];
end

always @(posedge clock) begin
  res_mant <= quot[24] ? quot[23:1] : quot[22:0];
end

always @(posedge clock)
  if (a_nan || b_nan || b_zero) begin
      quotient <= {res_sign, 8'hFF, 23'h1};
  end else if (a_zero || b_inf) begin
      quotient <= {res_sign, 8'h0, 23'h0};
  end else if (a_inf) begin
      quotient <= {res_sign, 8'hFF, 23'h0};
  end else if (res_exp[8] || (res_exp[7:0] > 254)) begin
      quotient <= {res_sign, 8'hFF, 23'h0};
  end else if (res_exp[7:0] < 1) begin
      quotient <= {res_sign, 8'h0, 23'h0};
  end else begin
      quotient <= {res_sign, res_exp[7:0], res_mant};
  end

assign remain = 0;

endmodule
