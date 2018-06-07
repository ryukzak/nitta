`timescale 1 ps / 1 ps

module div
  #( parameter DATA_WIDTH = 32
   , parameter PIPELINE   = 4
   )
  ( clock
  , denom 
  , numer
  , quotient
  , remain
  );

input                      clock;
input  [DATA_WIDTH-1:0]    denom;
input  [DATA_WIDTH-1:0]    numer;
output [DATA_WIDTH-1:0] quotient;
output [DATA_WIDTH-1:0]   remain;

reg [DATA_WIDTH-1:0] buf_quotient[PIPELINE-1:0];
reg [DATA_WIDTH-1:0] buf_remain  [PIPELINE-1:0];

integer i;
always @(posedge clock) begin
	buf_quotient[0] <= numer / denom;
	buf_remain[0] <= numer % denom;
	for ( i = 1; i < PIPELINE; i = i + 1 ) begin
		buf_quotient[i] <= buf_quotient[i-1];		
		buf_remain[i] <= buf_remain[i-1];
	end
end

wire [DATA_WIDTH-1:0] quotient = buf_quotient[PIPELINE-1];
wire [DATA_WIDTH-1:0] remain = buf_remain[PIPELINE-1];

endmodule
