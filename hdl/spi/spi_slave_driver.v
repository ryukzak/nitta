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
  // , output                  premature_ready
  , output [DATA_WIDTH-1:0] data_out // data written to slave in last transaction
  // SPI iterface
  , output           miso
  , input            mosi
  , input            sclk
  , input            cs
  );

reg [DATA_WIDTH-1:0] shiftreg;
reg miso_buf;

localparam STATE_WAIT_SCLK_1 = 0;
localparam STATE_WAIT_SCLK_0 = 1;
localparam STATE_READY = 2;
reg [1:0] state;

localparam DATA_COUNTER_WIDTH = $clog2( DATA_WIDTH + 1 );
reg [DATA_COUNTER_WIDTH-1:0] data_counter;

always @( posedge clk ) begin
  if ( rst || cs ) begin
    data_counter <= 0;
    state <= STATE_WAIT_SCLK_0;
    miso_buf <= 0;
  end else begin
    case ( state )

      STATE_WAIT_SCLK_1, STATE_READY: if ( sclk ) begin
        if ( data_counter == 0 || data_counter == DATA_WIDTH ) begin
          shiftreg <= data_in;
          miso_buf <= data_in[ DATA_WIDTH - 1 ];
        end else begin
          miso_buf <= shiftreg[ DATA_WIDTH - 1 ];
        end
        if ( data_counter == DATA_WIDTH) data_counter <= 1;
        else data_counter <= data_counter + 1;
        state <= STATE_WAIT_SCLK_0;
      end else if ( state == STATE_READY ) begin
        state <= STATE_WAIT_SCLK_1;
      end

      STATE_WAIT_SCLK_0: if ( !sclk ) begin
        shiftreg <= { shiftreg[DATA_WIDTH - 2:0], mosi } ;
        if ( data_counter == DATA_WIDTH ) state <= STATE_READY;
        else state <= STATE_WAIT_SCLK_1;
      end

      default: state <= STATE_WAIT_SCLK_0;
    endcase
  end
end

assign miso = state == STATE_WAIT_SCLK_1 && sclk
            ? ( data_counter == 0 || data_counter == DATA_WIDTH
              ? data_in[ DATA_WIDTH - 1 ]
              : shiftreg[ DATA_WIDTH - 1 ]
              )
            : miso_buf;

assign data_out = shiftreg;
assign ready = state == STATE_READY;


endmodule
