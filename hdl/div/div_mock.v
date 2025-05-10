`timescale 1 ps / 1 ps

module div
  #( parameter DATA_WIDTH = 32
   , parameter PIPELINE   = 4
   , parameter SCALING_FACTOR_POWER = 0
   )
  ( input signed clock
  , input signed [DATA_WIDTH-1:0] denom
  , input signed [DATA_WIDTH-1:0] numer
  , output signed [DATA_WIDTH-1:0] quotient
  , output signed [DATA_WIDTH-1:0] remain
  );

reg signed [DATA_WIDTH-1:0] buf_quotient[PIPELINE-1:0];
reg signed [DATA_WIDTH-1:0] buf_remain  [PIPELINE-1:0];

wire sign;
assign sign = (numer < 0) ^ (denom < 0);

wire [DATA_WIDTH + SCALING_FACTOR_POWER-1:0] shifted_numer;
assign shifted_numer = {numer < 0 ? -numer : numer, {SCALING_FACTOR_POWER{1'b0}}};

wire [DATA_WIDTH-1:0] abs_denom;
assign abs_denom = denom < 0 ? -denom : denom;


integer i;
always @(posedge clock) begin
  buf_quotient[0] <= sign ? -(shifted_numer / abs_denom) : (shifted_numer / abs_denom);
  buf_remain[0] <= numer % denom;
  for ( i = 1; i < PIPELINE; i = i + 1 ) begin
    buf_quotient[i] <= buf_quotient[i-1];
    buf_remain[i] <= buf_remain[i-1];
  end
end

assign quotient = buf_quotient[PIPELINE-1];
assign remain = buf_remain[PIPELINE-1];

endmodule
