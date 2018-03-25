module pu_mult 
  #( parameter DATA_WIDTH        = 32
   , parameter ATTR_WIDTH        = 4
   , parameter INVALID  			= 0 
   )
  ( input  wire                  clk
  , input  wire                  rst

  , input  wire                  signal_wr  
  , input  wire                  signal_sel
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH:0]   attr_in

  , input  wire                  signal_oe
  , output wire [DATA_WIDTH-1:0] data_out 
  , output wire [ATTR_WIDTH:0]   attr_out
  );


reg [DATA_WIDTH-1:0]         			stage1 [0:1];
reg                          			stage1_invalid [0:1];
reg [DATA_WIDTH-1:DATA_WIDTH/2-1]	stage_half[0:1]; 


always @(posedge clk) begin
	if ( rst ) begin
    stage1[0] <= 0;
    stage1[1] <= 0;
	end else begin 
    if ( signal_wr ) begin
      stage1[signal_sel] <= data_in[DATA_WIDTH-1:0];
		stage_half[signal_sel] <= data_in[DATA_WIDTH-1:DATA_WIDTH/2-1];
      stage1_invalid[signal_sel] <= attr_in[INVALID];
	  end
  end
end


function invalid_value1;
parameter DATA_WIDTH        		= 32; 
input [DATA_WIDTH-1:DATA_WIDTH/2-1] a;
input [DATA_WIDTH-1:DATA_WIDTH/2-1] b;
input c;
input d;

reg is_highhalf_value[0:1];
reg is_inv_highhalf_value[0:1];
reg operand_xor [0:1];
reg e1;
reg e2;

begin

 e1 = ~a == {(DATA_WIDTH/2+1){1'b0}}; 
 e2 = ~b == {(DATA_WIDTH/2+1){1'b0}};

is_highhalf_value[0] = (  a ==  0 );
is_inv_highhalf_value[0] = e1;


is_highhalf_value[1] = (  b ==  0 );
is_inv_highhalf_value[1] = e2;

operand_xor[0] = !(is_highhalf_value[0] ^ is_inv_highhalf_value[0]);
operand_xor[1] = !(is_highhalf_value[1] ^ is_inv_highhalf_value[1]); 
                              
invalid_value1 = operand_xor[0] || c
                  || operand_xor[1] || d;

end
endfunction


wire [DATA_WIDTH-1:0]         mult_result;
mult_inner mult_i1
  ( .dataa( stage1[0][DATA_WIDTH/2-1:0] )
  , .datab( stage1[1][DATA_WIDTH/2-1:0] )
  , .result( mult_result )
  );

reg                           f;
reg                           write_multresult;
reg 									invalid_value;

always @(posedge clk) begin
invalid_value <= invalid_value1(stage_half [0],stage_half [1],stage1_invalid[0],stage1_invalid[1]);
  if ( rst ) begin
    f <= 0;
    write_multresult <= 0;
  end else begin
    f <= signal_sel;
    if ( signal_sel == 0 && f == 1 ) write_multresult <= 1;
    else                             write_multresult <= 0;
  end
end

reg                           invalid_result;
reg [DATA_WIDTH-1:0]          data_multresult;

always @(posedge clk) begin
  if ( rst ) begin
    data_multresult <= 0;
    invalid_result <= 0;
  end else begin
    if ( write_multresult ) begin
      invalid_result <= invalid_value;
      data_multresult <= mult_result;
    end
  end
end

assign data_out = signal_oe ? data_multresult : 0;
assign attr_out = signal_oe ? ({ {(ATTR_WIDTH-1){1'b0}}, invalid_result } << INVALID) 
                            | {(ATTR_WIDTH-1){1'b0}} 
                            : 0;

endmodule



	