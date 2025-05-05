

module float_sum 
  #( parameter DATA_WIDTH = 32
  )
  ( input wire [DATA_WIDTH-1:0] a
  , input wire [DATA_WIDTH-1:0] b
  , output reg [DATA_WIDTH-1:0] result
  , output wire attr
);
  
  wire sign_a = a[31];
  wire [7:0] exp_a = a[30:23];
  wire [23:0] mant_a = {1'b1, a[22:0]};
  
  wire sign_b = b[31];
  wire [7:0] exp_b = b[30:23];
  wire [23:0] mant_b = {1'b1, b[22:0]};

  wire a_zero = (exp_a == 0) && (mant_a[22:0] == 0);
  wire b_zero = (exp_b == 0) && (mant_b[22:0] == 0);
  wire a_inf  = (exp_a == 8'hFF) && (mant_a[22:0] == 0);
  wire b_inf  = (exp_b == 8'hFF) && (mant_b[22:0] == 0);
  wire a_nan  = (exp_a == 8'hFF) && (mant_a[22:0] != 0);
  wire b_nan  = (exp_b == 8'hFF) && (mant_b[22:0] != 0);

  wire [7:0] max_exp = (exp_a > exp_b) ? exp_a : exp_b;
  
  wire [23:0] mant_a_aligned = (exp_a > exp_b) ? mant_a : mant_a >> (exp_b - exp_a);
    
  wire [23:0] mant_b_aligned = (exp_b > exp_a) ? mant_b : mant_b >> (exp_a - exp_b);

  reg [24:0] sum_mant;
  always @*
    sum_mant <= (sign_a == sign_b) ? 
    mant_a_aligned + mant_b_aligned : 
    (mant_a_aligned > mant_b_aligned) ? 
    mant_a_aligned - mant_b_aligned : 
    mant_b_aligned - mant_a_aligned;
    
  wire res_sign = (sign_a == sign_b) ? sign_a : 
    (mant_a_aligned > mant_b_aligned) ? sign_a : sign_b;

  reg [7:0] shift;
  always @(sum_mant) begin
    if (sum_mant[24]) begin
      shift = 0;
    end else if (sum_mant[23]) begin
      shift = 1;
    end else if (sum_mant[22]) begin
      shift = 2;
    end else if (sum_mant[21]) begin
      shift = 3;
    end else if (sum_mant[20]) begin
        shift = 4;
    end else if (sum_mant[19]) begin
        shift = 5;
    end else if (sum_mant[18]) begin
        shift = 6;
    end else if (sum_mant[17]) begin
        shift = 7;
    end else if (sum_mant[16]) begin
        shift = 8;
    end else if (sum_mant[15]) begin
        shift = 9;
    end else if (sum_mant[14]) begin
        shift = 10;
    end else if (sum_mant[13]) begin
        shift = 11;
    end else if (sum_mant[12]) begin
        shift = 12;
    end else if (sum_mant[11]) begin
        shift = 13;
    end else if (sum_mant[10]) begin
        shift = 14;
    end else if (sum_mant[9]) begin
        shift = 15;
    end else if (sum_mant[8]) begin
        shift = 16;
    end else if (sum_mant[7]) begin
        shift = 17;
    end else if (sum_mant[6]) begin
        shift = 18;
    end else if (sum_mant[5]) begin
        shift = 19;
    end else if (sum_mant[4]) begin
        shift = 20;
    end else if (sum_mant[3]) begin
        shift = 21;
    end else if (sum_mant[2]) begin
        shift = 22;
    end else if (sum_mant[1]) begin
        shift = 23;
    end else if (sum_mant[0]) begin
        shift = 24;
    end else begin
        shift = 0;
    end
  end
    


  wire [24:0] normalized_mant = sum_mant << shift;
  wire [7:0] new_exp = max_exp - shift + 1;

  wire overflow, underflow;

  assign overflow = (new_exp >= {8{1'b1}});
  assign underflow = (new_exp == 0);

  always @* begin
    if (a_nan || b_nan) begin
        result = {res_sign, 8'hFF, 23'h1};
    end else if ((a_inf || b_inf)) begin
        result = {res_sign, 8'hFF, 23'h0};
    end else if (a_zero) begin
        result = b;
    end else if (b_zero) begin
        result = a;
    end else if (new_exp[7:0] > 254) begin
        result = {res_sign, 8'hFF, 23'h0};
    end else if (new_exp[7:0] < 1) begin
        result = {res_sign, 8'h0, 23'h0};
    end else begin
        result = {res_sign, new_exp[7:0], normalized_mant[23:1]};
    end
  end

assign attr = overflow;

endmodule