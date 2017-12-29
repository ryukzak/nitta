module pu_spi
  #( parameter DATA_WIDTH = 8
 ,   parameter SCLK_HALFPERIOD = 1
   )
  ( input             clk
  , input             rst
  // system interface
  , input  [DATA_WIDTH-1:0] data_in           // data that master will write to slave
  , output [DATA_WIDTH-1:0] data_out          // data recevied from slave in last transaction
  , output                  ready             // transaction is not processed now
  // SPI interface
  , output wire       miso
  , output            mosi
  , input             sclk
  , input             cs
  );

localparam BIT_COUNTER_WIDTH = $clog2(DATA_WIDTH);
localparam SCLK_COUNTER_WIDTH = $clog2(SCLK_HALFPERIOD);

reg   [BIT_COUNTER_WIDTH-1:0] bit_count;
reg   [SCLK_COUNTER_WIDTH-1:0] sclk_count;

localparam STATE_IDLE        = 0;
localparam STATE_WAIT_SCLK_1 = 1;
localparam STATE_WAIT_SCLK_0 = 2;
localparam STATE_FINALIZE    = 3;
reg  [2:0] state;

// Кастыль
assign slave_spi_mosi = mosi;
assign slave_spi_sclk = sclk;
assign slave_spi_cs   = cs;
assign slave_ready    = ready;
assign miso = slave_spi_miso;

spi_slave_driver pu_spi_inner ( 
	.clk(clk)
  , .rst(rst)
  
  , .data_in(data_in)
  , .ready(slave_ready)
  , .data_out(data_out)
  
  , .mosi(slave_spi_mosi)
  , .miso(slave_spi_miso)
  , .sclk(slave_spi_sclk)
  , .cs(slave_spi_cs)
  );

always @( posedge rst, posedge clk ) begin
  if ( rst ) begin
    bit_count <= 0;
    state <= STATE_IDLE;
  end
  else begin
    case ( state )
      STATE_IDLE: begin
          bit_count <= 7;
          state <= STATE_WAIT_SCLK_1;
      end
      STATE_WAIT_SCLK_1: begin
        if ( !sclk_count ) begin
          //miso <= slave_spi_miso;
          state <= STATE_WAIT_SCLK_0;
        end 
      end
      STATE_WAIT_SCLK_0: begin
        if ( !sclk_count && bit_count ) begin          
          bit_count <= bit_count - 1;
          state <= STATE_WAIT_SCLK_1;
        end 
        else if ( !sclk_count ) begin
          state <= STATE_FINALIZE;
        end
      end
      STATE_FINALIZE: begin
        if ( !sclk_count ) begin 
          state <= STATE_IDLE;
        end
      end
      default: state <= STATE_IDLE;
    endcase        
  end
end

always @(posedge clk) begin
  if ( sclk_count ) sclk_count <= sclk_count - 1;
  else              sclk_count <= SCLK_HALFPERIOD - 1;
end

endmodule
