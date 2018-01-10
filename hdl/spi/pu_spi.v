module pu_spi
  #( parameter DATA_WIDTH       = 8
,    parameter NUMBER_OF_BUFFER = 3 
   )
  ( input             clk
  , input             rst
  // system interface
  , input  [DATA_WIDTH-1:0] data_in  
  , output [DATA_WIDTH-1:0] data_out 
  , output                  ready
  , output            oe
  , output            wr
  , input             cycle // Сигнал о начале вычислительного цикла
  , output reg        start // Выходной сигнал о начале цикла передачи данных
  , output reg        stop  // Выходной сигнал о конце цикла передачи данных
  // SPI interface
  , output wire       miso
  , output            mosi
  , input             sclk
  , input             cs
  );

localparam ADDR_WIDTH_BUF  = $clog2( NUMBER_OF_BUFFER );

wire [DATA_WIDTH-1:0] data_in_pu_spi   [ADDR_WIDTH_BUF:0];
wire [DATA_WIDTH-1:0] data_out_pu_spi  [ADDR_WIDTH_BUF:0];
reg                   flag;
reg [1:0]             count_cycle;

genvar index;
generate  
  for ( index = 0 ; index < NUMBER_OF_BUFFER ; index = index + 1 )  
  begin: gen_buffer
    pu_buffer buffer (
      .clk(clk)
    , .rst(rst) 
    , .oe(oe)
    , .wr(wr)
    , .data_in(data_out_pu_spi[index])
    , .data_out(data_in_pu_spi[index])
    );
  end
endgenerate 

always @(posedge clk or posedge rst) begin
  if ( rst ) begin
    flag <= 0;
  end else begin
    if ( cycle ) begin
      flag = 1;
    end else begin
      if ( flag ) begin

        // Организовать работу
        // flag = 0;
        
      end
    end
  end
end

// Переключение буферов (count_cycle используется вместо переменной index)
always @(posedge cycle) begin
  count_cycle <= count_cycle + 1'b1;
end

endmodule