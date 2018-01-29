module play 
#(parameter DATA_WIDTH 			= 	8,
  parameter ATTR_WIDTH 			= 	4,
  parameter VALID_INVALID_IN 	= 	0,
  parameter VALID_INVALID_OUT = 	0)
(
	input wire 							clk,
	input wire							RST,  
	input wire							WR,
	input wire							OE,
	input wire							SEL,
	input wire 	[DATA_WIDTH-1:0] 	data_in,
	input wire	[ATTR_WIDTH:0]		attr_in,
	output reg	[ATTR_WIDTH:0]		attr_out, 
	output reg  [DATA_WIDTH-1:0] 	data_out 
  );

reg [DATA_WIDTH-1:0] 				operand1 [1:0];
reg [DATA_WIDTH-1:0] 				operand2 [1:0];
reg invalid_value;
reg [DATA_WIDTH-1:DATA_WIDTH/2] 	INV [1:0];
wire [DATA_WIDTH-1:0] 				mult_result;
reg 										f;
reg 										write_multresult;
reg 										invalid_result;
reg [DATA_WIDTH-1:0] 				data_multresult;

//wire clk;
//pll pll(
//  .inclk0(CLOCK_50),
//  .c0(clk)
//  );

pu_mult_inner mult_i1(
	.dataa(operand2 [0]),
	.datab(operand2 [1]),
	.result(mult_result));
	


always @(posedge clk) begin
	 if (RST) begin
		attr_out = 0;
		data_out = 0;
		operand1[0] = 0;
		operand1[1] = 0;
		operand2[0] = 0;
		operand2[1] = 0;
		f <= 0;
		invalid_value = 0;
		write_multresult <= 0;
		data_multresult <= 0;
		invalid_result <= 0;
	 end

	
	f <= SEL;
	if ( SEL == 0 & f == 1) begin
		write_multresult <= 1;
	end 
	else begin
		write_multresult <= 0;
	end


	 if (WR) begin
		case (SEL)
			0:operand1[0] = data_in [DATA_WIDTH-1:0];
			1:operand1[1] = data_in [DATA_WIDTH-1:0];
		endcase
		INV[0] = ~operand1[0][DATA_WIDTH-1:DATA_WIDTH/2-1];
		INV[1] = ~operand1[1][DATA_WIDTH-1:DATA_WIDTH/2-1];
		if (attr_in[VALID_INVALID_IN] == 0 & SEL == 1) begin
			operand2[0] = operand1[0];
			operand2[1] = operand1[1];
			invalid_value = 1;
		end
		if (attr_in[VALID_INVALID_IN] == 1 & SEL == 1) begin
			if (((operand1[0][DATA_WIDTH-1:DATA_WIDTH/2] == 0) && (operand1[1][DATA_WIDTH-1:DATA_WIDTH/2] == 0)) || 
		 ((INV[0] == 0) && (INV[1] == 0))||((INV[0] == 0) && (operand1[1][DATA_WIDTH-1:DATA_WIDTH/2] == 0))||
		 ((INV[1] == 0) && (operand1[0][DATA_WIDTH-1:DATA_WIDTH/2] == 0))) begin
		
			operand2[0] = operand1[0];
			operand2[1] = operand1[1];
			invalid_value = 1;
				
			end 
			else begin
		
			operand2[0] = operand1[0];
			operand2[1] = operand1[1];
			invalid_value = 0;
			end
		end
	 end
	
	if (write_multresult) begin
		invalid_result <= invalid_value;
		data_multresult <= mult_result;
	end

	
	if (OE) begin
		data_out <= data_multresult;
		attr_out[VALID_INVALID_OUT] <= invalid_result;
	end 
	else begin 
		data_out <= 0;
		attr_out[VALID_INVALID_OUT] <= 0;
	end
end

endmodule
	
	