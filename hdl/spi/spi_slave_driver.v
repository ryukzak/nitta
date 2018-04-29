`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Simple SPI master controller with CPOL=0, CPHA=1
//////////////////////////////////////////////////////////////////////////////////

module spi_slave_driver 
  #( parameter DATA_WIDTH = 8
   , parameter SUB_FRAME = 0
   )
  ( input            clk
  , input            rst
  // system interface
  , input  [DATA_WIDTH-1:0] data_in  // data that master can read from slave
  , output                  ready    // transaction is not processed now 
  , output                  premature_ready
  , output [DATA_WIDTH-1:0] data_out // data written to slave in last transaction
  // SPI iterface
  , output reg       miso
  , input            mosi
  , input            sclk
  , input            cs
  );

reg   [DATA_WIDTH-1:0] shiftreg;

localparam STATE_IDLE = 0;        // wait for transaction begin
localparam STATE_WAIT_SCLK_1 = 1; // wait for SCLK to become 1
localparam STATE_WAIT_SCLK_0 = 2; // wait for SCLK to become 0
localparam STATE_DELAY = 3;
reg   [1:0] state;

localparam DATA_COUNTER_WIDTH  = $clog2( DATA_WIDTH + 1 );
reg   [DATA_COUNTER_WIDTH-1:0] data_counter;

always @( posedge rst, posedge clk ) begin
  if ( rst ) begin
    shiftreg <= 0;
    miso <= 0;
    data_counter <= 0;
    state <= STATE_IDLE;
  end else begin
    if ( cs ) begin
      data_counter <= 0;
      state <= STATE_IDLE;
    end else begin
      case ( state )
        STATE_IDLE: begin
          shiftreg <= data_in; 
          data_counter <= 0;        
          state <= STATE_WAIT_SCLK_1;
        end
        STATE_WAIT_SCLK_1: begin
          if ( sclk ) begin
            miso <= shiftreg[ DATA_WIDTH - 1 ];
            if ( data_counter == DATA_WIDTH ) begin
              data_counter <= 1;
            end else begin
              data_counter <= data_counter + 1;   
            end
            state <= STATE_WAIT_SCLK_0;           
          end
        end
        STATE_WAIT_SCLK_0, STATE_DELAY: begin
          if ( !sclk ) begin
            if ( SUB_FRAME && data_counter == DATA_WIDTH ) begin
              if ( state == STATE_WAIT_SCLK_0 ) begin
                shiftreg <= { shiftreg[DATA_WIDTH - 2:0], mosi } ;
                state <= STATE_DELAY;
              end else begin
                shiftreg <= data_in;
                state <= STATE_WAIT_SCLK_1;
              end
            end else begin
              shiftreg <= { shiftreg[DATA_WIDTH - 2:0], mosi } ;
              state <= STATE_WAIT_SCLK_1;              
            end
          end
        end
        default: state <= STATE_IDLE;
      endcase
    end
  end
end

assign data_out = shiftreg;
assign ready = state == STATE_IDLE
            || data_counter == DATA_WIDTH && sclk && state == STATE_WAIT_SCLK_1 ;
assign premature_ready = state == STATE_IDLE
                      || data_counter == DATA_WIDTH && !sclk && ( state == STATE_WAIT_SCLK_0 ) ;
            
  
endmodule
