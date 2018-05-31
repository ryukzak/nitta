module div (
	clock,
	denom,
	numer,
	quotient,
	remain);

	input	  clock;
	input	[31:0]  denom;
	input	[31:0]  numer;
	output	[31:0]  quotient;
	output	[31:0]  remain;

endmodule