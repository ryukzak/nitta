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

wire [DATA_WIDTH + SCALING_FACTOR_POWER:0] shifted_numer;
assign shifted_numer = {numer, {SCALING_FACTOR_POWER{1'b0}}};

integer i;
always @(posedge clock) begin
  buf_quotient[0] <= shifted_numer / denom;
  buf_remain[0] <= numer % denom;
  for ( i = 1; i < PIPELINE; i = i + 1 ) begin
    buf_quotient[i] <= buf_quotient[i-1];
    buf_remain[i] <= buf_remain[i-1];
  end
end

assign quotient = buf_quotient[PIPELINE-1];
assign remain = buf_remain[PIPELINE-1];

endmodule
