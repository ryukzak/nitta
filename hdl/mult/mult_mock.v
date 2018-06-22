module mult_inner (
	dataa,
	datab,
	result);

	input	signed [15:0]  dataa;
	input	signed [15:0]  datab;
	output signed [31:0]  result;

assign result = dataa * datab;

endmodule
