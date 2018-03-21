`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Simple SPI master controller with CPOL=0, CPHA=1
//////////////////////////////////////////////////////////////////////////////////

module spi_slave_driver 
  #( parameter DATA_WIDTH = 32
   )
  ( input            clk
  , input            rst
  // system interface
  , input  [DATA_WIDTH-1:0] data_in  // data that master can read from slave
  , output                  ready    // transaction is not processed now 
  , output [DATA_WIDTH-1:0] data_out // data written to slave in last transaction
  // SPI iterface
  , output reg       miso
  , input            mosi
  , input            sclk
  , input            cs
  );

reg   [DATA_WIDTH-1:0] shiftreg;

localparam STATE_IDLE = 0; // wait for transaction begin
localparam STATE_WAIT_SCLK_1 = 1; // wait for SCLK to become 1
localparam STATE_WAIT_SCLK_0 = 2; // wait for SCLK to become 0
reg   [2:0] state;

always @( posedge rst, posedge clk ) begin
  if ( rst ) begin
    shiftreg <= 0;
    miso <= 0;
    state <= STATE_IDLE;
  end 
  else begin
    if ( cs ) begin
      state <= STATE_IDLE;
    end
    else begin
      case ( state )
        STATE_IDLE: begin
          shiftreg <= data_in;
          state <= STATE_WAIT_SCLK_1;
        end
        STATE_WAIT_SCLK_1: begin
          if ( sclk ) begin
            miso <= shiftreg[ DATA_WIDTH - 1 ];
            state <= STATE_WAIT_SCLK_0;
          end
        end
        STATE_WAIT_SCLK_0: begin
          if ( !sclk ) begin
            shiftreg <= { shiftreg[DATA_WIDTH - 2:0], mosi };
            state <= STATE_WAIT_SCLK_1;
          end
        end
        default: state <= STATE_IDLE;
      endcase
    end
  end
end

assign { ready, data_out } = { state == STATE_IDLE, shiftreg };
  
endmodule
