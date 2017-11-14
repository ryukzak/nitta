`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Simple SPI master controller with CPOL=0, CPHA=1
//////////////////////////////////////////////////////////////////////////////////

module spi_slave_driver(
  input            clk,
  input            rst,
  
  // system interface
  input      [7:0] data_in,  // data that master can read from slave
  output reg       ready,     // transaction is not processed now 
  output reg [7:0] data_out, // data written to slave in last transaction
  
  // SPI iterface
  output reg       miso,
  input            mosi,
  input            sclk,
  input            cs
  );

reg   [7:0] shiftreg;
// reg bit_buf;

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
            miso <= shiftreg[7];
            state <= STATE_WAIT_SCLK_0;
          end
        end
        STATE_WAIT_SCLK_0: begin
          if ( sclk == 0 ) begin
            shiftreg <= { shiftreg[6:0], mosi };
            state <= STATE_WAIT_SCLK_1;
          end
        end
        default: state <= STATE_IDLE;
      endcase
    end
  end
end

always @( posedge clk ) begin
  { ready, data_out } = { state == STATE_IDLE, shiftreg };
end
  
endmodule
