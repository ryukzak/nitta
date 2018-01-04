`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Simple SPI master controller with CPOL=0, CPHA=1
//////////////////////////////////////////////////////////////////////////////////

module spi_master_driver 
  #( parameter DATA_WIDTH = 8
   , parameter SCLK_HALFPERIOD = 1
   )
  ( input             clk
  , input             rst
  // system interface
  , input                   start_transaction // signal to start transaction
  , input  [DATA_WIDTH-1:0] data_in           // data that master will write to slave
  , output [DATA_WIDTH-1:0] data_out          // data recevied from slave in last transaction
  , output                  ready             // transaction is not processed now
  // SPI interface
  , output            mosi
  , input             miso
  , output reg        sclk
  , output reg        cs // want to be selector...
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

spi_slave_driver  
  #( .DATA_WIDTH(DATA_WIDTH)
   ) inner
  ( .clk(clk)
  , .rst(rst)
  
  , .data_in(data_in)
  , .ready(ready)
  , .data_out(data_out)
  
  , .mosi(miso)
  , .miso(mosi)
  , .sclk(sclk)
  , .cs(cs)
  );

always @( posedge rst, posedge clk ) begin
  if ( rst ) begin
    bit_count <= 0;
    sclk <= 0;
    cs <= 1;
    state <= STATE_IDLE;
  end
  else begin
    case ( state )
      STATE_IDLE: begin
        sclk <= 0;
        if ( start_transaction ) begin
          bit_count <= 7;
          cs <= 0;
          state <= STATE_WAIT_SCLK_1;
        end 
        else cs <= 1;
      end
      STATE_WAIT_SCLK_1: begin
        if ( !sclk_count ) begin
          sclk <= 1;
          state <= STATE_WAIT_SCLK_0;
        end 
      end
      STATE_WAIT_SCLK_0: begin
        if ( !sclk_count && bit_count ) begin
          sclk <= 0;
          bit_count <= bit_count - 1;
          state <= STATE_WAIT_SCLK_1;
        end 
        else if ( !sclk_count ) begin
          sclk <= 0;
          state <= STATE_FINALIZE;
        end
      end
      STATE_FINALIZE: begin
        if ( !sclk_count ) begin 
          cs <= 1;
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
