 module pu_mult // INFO: принято что название модуля совпадало с именем файла.
  #( parameter DATA_WIDTH        = 32
   , parameter ATTR_WIDTH        = 4
   , parameter INVALID  			= 0 // FIXME: Заменить на один параметр под именем INVALID
   )
  ( input  wire              clk
  , input  wire              rst

  , input  wire                  signal_wr  // INFO: Для унификации интерфейсов с другими pu.
  , input  wire                  signal_sel
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH:0]   attr_in

  , input  wire                  signal_oe
  , output wire [DATA_WIDTH-1:0] data_out 
  , output wire [ATTR_WIDTH:0]   attr_out
  );


reg [DATA_WIDTH-1:0]         stage1 [0:1];
reg                          stage1_invalid [0:1];

always @(posedge clk) begin
	if ( rst ) begin
    stage1[0] <= 0;
    stage1[1] <= 0;
	end else begin 
    if ( signal_wr ) begin
      stage1[signal_sel] <= data_in[DATA_WIDTH-1:0];
      stage1_invalid[signal_sel] <= attr_in[INVALID];
	  end
  end
end


wire is_operand_invalid[0:1];
assign is_operand_invalid[0] = !(  stage1[0][DATA_WIDTH-1:DATA_WIDTH/2-1] == 0
                                ^ ~stage1[0][DATA_WIDTH-1:DATA_WIDTH/2-1] == 0
                                ) || stage1_invalid[0];
assign is_operand_invalid[1] = !(  stage1[1][DATA_WIDTH-1:DATA_WIDTH/2-1] == 0
                                ^ ~stage1[1][DATA_WIDTH-1:DATA_WIDTH/2-1] == 0
                                ) || stage1_invalid[1];
wire invalid_value = !is_operand_invalid[0] 
                  || !is_operand_invalid[1];

wire [DATA_WIDTH-1:0]         mult_result;
mult_inner mult_i1
  ( .dataa( stage1[0][DATA_WIDTH/2-1:0] )
  , .datab( stage1[1][DATA_WIDTH/2-1:0] )
  , .result( mult_result )
  );



reg                           f;
reg                           write_multresult;

always @(posedge clk) begin
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
                            | {ATTR_WIDTH{1'b0}} 
                            : 0;

endmodule

	