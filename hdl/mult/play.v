module pu_mult // INFO: принято что название модуля совпадало с именем файла.
  #( parameter DATA_WIDTH        = 8
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

reg [DATA_WIDTH-1:0]         operand1 [1:0];
reg [DATA_WIDTH-1:0]         operand2 [1:0];
reg [DATA_WIDTH-1:DATA_WIDTH/2]   inv [1:0]; // INFO: что бы не путать с параметрами.
reg                           invalid_value;
wire [DATA_WIDTH-1:0]         mult_result;
reg                           f;
reg                           write_multresult;
reg                           invalid_result;
reg [DATA_WIDTH-1:0]          data_multresult;
wire op_1 = operand1[0][DATA_WIDTH-1:DATA_WIDTH/2] ;
wire op_2 = operand1[1][DATA_WIDTH-1:DATA_WIDTH/2] ;

mult mult_i1
  ( .dataa( operand2[0] )
  , .datab( operand2[1] )
  , .result( mult_result )
  );
  
always @(posedge clk) begin
	if ( rst ) begin
    // FIXME: Следует использовать присваивание только одного типа (<=), так как в таком случае кодв целом становится проще.
    // При этом, в рамках одного always блока не стоит делать цепочки вида: b <= a; c <= b;
    operand1[0] = 0;
    operand1[1] = 0;
    operand2[0] = 0;
    operand2[1] = 0;
    f <= 0;
    invalid_value = 0;
    write_multresult <= 0;
    data_multresult <= 0;
    invalid_result <= 0;
	end else begin // INFO: в таком виде код однозначнее и понятнее синтезатору.

    f <= signal_sel;
    if ( signal_sel == 0 && f == 1 ) begin // INFO: Не смотря на то что функционально оба оператора работают 
                                           // одинаково в данном случае, по смыслу тут логическое и, а не побитовое.
      write_multresult <= 1;
    end else begin
      write_multresult <= 0;
    end

    if ( signal_wr ) begin
      case ( signal_sel )
        0: operand1[0] = data_in[DATA_WIDTH-1:0];
		  0: inv[0] = ~operand1[0][DATA_WIDTH-1:DATA_WIDTH/2-1];
        1: operand1[1] = data_in[DATA_WIDTH-1:0];
		  1: inv[1] = ~operand1[1][DATA_WIDTH-1:DATA_WIDTH/2-1];
      endcase
      // FIXME: Почему это не в case? Мне кажется логичнее перенести это туда. 

      // FIXME: Этот и следующий if в совокупности я не понимаю. Операнды у вас присваиваются одинаково во всех if-ах.
      // invalid_value корректно устанавливается только во втором.
      // Их следует слить воедино.

      if ( attr_in[INVALID] == 0 && signal_sel == 1 ) begin
        // FIXME: Вынесте эту логику на именованные провода, что бы еёможно было воспринемать.
			if (((op_1 == 0) && (op_2 == 0)) || ((inv[0] == 0) && (inv[1] == 0))||((inv[0] == 0) && (op_2 == 0))||((inv[1] == 0) && (op_1 == 0))) begin  
				operand2[0] = operand1[0];
				operand2[1] = operand1[1];
				invalid_value = 0;
				end 
				else begin 
				operand2[0] = operand1[0];
				operand2[1] = operand1[1];
				invalid_value = 1;
				end
			end
			else if ( attr_in[INVALID] == 1 && signal_sel == 1 ) begin
				operand2[0] = operand1[0];
				operand2[1] = operand1[1];
				invalid_value = 1;
			end
	end

    if ( write_multresult ) begin
      invalid_result <= invalid_value;
      data_multresult <= mult_result;
    end

    // INFO: Не вижу смысла в том, что бы выходные порты были регистрами,а такт это съедает. По этому переделал на провода. 
    // Смотрите в интерфейсе и после этого always-а.
    // if ( signal_oe ) begin
    //   data_out <= data_multresult;
    //   attr_out[VALID_INVALID_OUT] <= invalid_result;
    // end else begin 
    //   data_out <= 0;
    //   attr_out[VALID_INVALID_OUT] <= 0;
    // end
  end
end

assign data_out = signal_oe ? data_multresult : 0;
assign attr_out = signal_oe ? ({{(ATTR_WIDTH-1){1'b0}},invalid_result} << INVALID) | {ATTR_WIDTH{1'b0}} : 0;

endmodule

	