module pu_spi
  #( parameter DATA_WIDTH       = 8
  ,  parameter ATTR_WIDTH       = 4
  ,  parameter VALID            = 1
  ,  parameter BUFFER_SIZE      = 3
   )
  ( input                   clk
  , input                   rst

  // system interface
  , input  wire                  signal_wr 
  , input  wire [DATA_WIDTH-1:0] data_in  
  , input  wire [ATTR_WIDTH-1:0] attr_in 

  , input  wire                  signal_oe 
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out 

  , input  wire       flag_cycle 
  , output reg        flag_start
  , output reg        flag_stop

  // SPI interface
  , output wire       miso
  , output wire       mosi
  , input  wire       sclk
  , input  wire       cs
);

localparam BUFFER_SIZE_WIDTH = $clog2( BUFFER_SIZE );
reg  [BUFFER_SIZE_WIDTH-1:0] actual_buffer_sending_size;
reg  [BUFFER_SIZE_WIDTH-1:0] actual_buffer_receiving_size;

wire [DATA_WIDTH-1:0] data_out_spi;
reg  [DATA_WIDTH-1:0] data_in_spi;
wire buffer_full;
reg flag_wr;
reg flag_oe;
reg work_process;

pu_buffer buffer(
  .clk(clk)
, .rst(rst)
, .oe(signal_oe)
, .wr(signal_wr)
, .data_in(data_in_spi)
, .data_out(data_out_spi)
, .buffer_full(buffer_full)
); 

always @( posedge clk or posedge rst ) begin
  if ( rst ) begin
    flag_start <= 0;
    flag_stop  <= 0;
    actual_buffer_sending_size <= 0; 
    actual_buffer_receiving_size <= 0;
    flag_wr <= 0;
    flag_oe <= 0; 
    work_process <= 0;
  end else begin
    // Пришел сигнал на запись и буфер не заполнен
    if ( signal_wr && ~buffer_full ) begin
      data_in_spi <= data_in;
      if ( flag_start ) begin        
        actual_buffer_sending_size <= actual_buffer_sending_size + 1;
      end 
    end else if ( signal_wr && buffer_full ) begin 
      // Если буфер заполнен и есть сигнал на запись новых значений
    end
  end
  // Пришел сигнал на чтение и буфер не пуст
  if ( signal_oe && !(actual_buffer_sending_size == 0) ) begin
    data_out <= data_out_spi;
    if ( flag_start ) begin      
      actual_buffer_receiving_size <= actual_buffer_receiving_size + 1;
    end
  end
end

always @( posedge clk ) begin
  // Для синхронизации. Работаем по сигналу flag_cycle
  if ( flag_cycle ) begin
    work_process <= 1;
  end
  if ( work_process ) begin
    // Тута вся работа
  end
end

// Переделать логику. Дописать flag_stop
always @( posedge clk ) begin
  if ( signal_wr && signal_oe ) begin
   // Случай если одновременно Send and Recive
  end else if ( signal_wr ) begin
    if ( !flag_wr ) begin
      flag_start <= 1;
      flag_oe <= 0;
      flag_wr <= 1;
    end else begin
      flag_start <= 0;
    end
  end else if ( signal_oe ) begin
    if ( !flag_oe ) begin
      flag_start <= 1;
      flag_oe <= 1;
    end else begin
      flag_start <= 0;
    end
  end else begin
    flag_start <= 0;
    flag_wr <= 0;
    flag_oe <= 0;
  end
end

endmodule
