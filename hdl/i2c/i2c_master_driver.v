module i2c_master_driver
  #( parameter I2C_DATA_WIDTH = 8
   , parameter DATA_WIDTH     = 32
   , parameter ATTR_WIDTH     = 4
   , parameter ADDRES_DEVICE  = 7'h17
   )
  ( input                       clk
  , input                       rst
  // system interface
  , input                       start_transaction
  , input                       rw                 /* 1 read, 0 write */
  , input  [I2C_DATA_WIDTH-1:0] data_in
  , output [I2C_DATA_WIDTH-1:0] data_out
  , output reg                  ready

  // i2c interface
  , output reg                  scl
  , inout                       sda
  );

localparam STATE_IDLE       = 0;
localparam STATE_FINALIZE   = 1;
localparam STATE_START      = 2;
localparam STATE_SCL_LOW    = 3;
localparam STATE_SCL_HIGH   = 4;
reg [2:0] state_scl;
reg stop_transaction;
reg wait_scl;
reg start_stop;

always @( posedge rst, negedge clk ) begin
  if ( rst ) begin
    scl          <= 1;
    start_stop   <= 1;
    state_scl    <= STATE_IDLE;
  end
  else begin
    case ( state_scl )
      STATE_IDLE: begin
        scl <= 1;
        start_stop <= 1;
        if ( start_transaction ) begin
          state_scl <= STATE_START;
        end
      end
      STATE_START: begin
        start_stop <= 0;
        state_scl <= STATE_SCL_LOW;
      end
      STATE_SCL_LOW: begin
        scl <= 0;
        state_scl <= stop_transaction ? STATE_FINALIZE : wait_scl ? STATE_SCL_LOW : STATE_SCL_HIGH;
      end
      STATE_SCL_HIGH: begin
        scl <= 1;
        state_scl <= STATE_SCL_LOW;
      end
      STATE_FINALIZE: begin
        scl <= 1;
        state_scl <= STATE_IDLE;
      end
      default: state_scl <= STATE_IDLE;
    endcase
  end
end

localparam STATE_SEND_ADDRES    = 2;
localparam STATE_RECEIVE_ACK    = 3;
localparam STATE_SEND_BYTE      = 4;
localparam STATE_RECEIVE_BYTE   = 5;
localparam STATE_SEND_ACK       = 6;
reg  [2:0] state_ms;
reg highz_mode;

reg sda_o;
reg [I2C_DATA_WIDTH-1:0] shiftreg;

localparam DATA_COUNTER_WIDTH = $clog2( I2C_DATA_WIDTH + 1 );
reg [DATA_COUNTER_WIDTH-1:0] data_counter;

localparam BYTE_COUNTER_WIDTH = $clog2( DATA_WIDTH / 8 + 2 );
reg [BYTE_COUNTER_WIDTH-1:0] byte_counter;

always @(posedge rst, posedge clk) begin
  if ( rst ) begin
    sda_o <= 0;
    highz_mode <= 0;
    data_counter <= 0;
    byte_counter <= 0;
    wait_scl <= 0;
    ready <= 0;
    stop_transaction <= 0;
    state_ms <= STATE_IDLE;
  end
  else begin
    case ( state_ms )
      STATE_IDLE: begin
        if ( start_transaction ) begin
          shiftreg <= { ADDRES_DEVICE, rw };
          data_counter <= 0;
          byte_counter <= 0;
          ready <= 0;
          state_ms <= STATE_SEND_ADDRES;
        end
      end
      STATE_SEND_ADDRES, STATE_SEND_BYTE: begin
        if (!scl) begin
          shiftreg <= {shiftreg[I2C_DATA_WIDTH - 2:0], 1'b0};
          sda_o <= shiftreg[I2C_DATA_WIDTH-1];
          data_counter <= data_counter + 1;
          if ( data_counter == I2C_DATA_WIDTH ) begin
            highz_mode <= 1;
            sda_o <= 0;
            byte_counter <= byte_counter + 1;
            ready <= 1;
            state_ms <= STATE_RECEIVE_ACK;
          end
        end
      end
      STATE_RECEIVE_ACK: begin
        if (!sda && !scl) begin
          highz_mode <= rw;
          data_counter <= 0;
          shiftreg <= data_in;
          wait_scl <= 0;
          ready <= 0;
          state_ms <= rw ? STATE_RECEIVE_BYTE : STATE_SEND_BYTE;
        end else
        if (byte_counter == DATA_WIDTH / 8 + 1) begin
          highz_mode <= 0;
          stop_transaction <= 1;
          sda_o <= 0;
          wait_scl <= 0;
          state_ms <= STATE_FINALIZE;
        end else begin
          wait_scl <= 1;
        end
      end
      STATE_RECEIVE_BYTE: begin
        if (scl) begin
          data_counter <= data_counter + 1;
          shiftreg <= {shiftreg[I2C_DATA_WIDTH - 2:0], sda};
        end else begin
          if ( data_counter == I2C_DATA_WIDTH ) begin
            highz_mode <= 0;
            sda_o <= byte_counter == DATA_WIDTH / 8 ? 1 : 0; // here
            ready <= 1;
            byte_counter <= byte_counter + 1;
            state_ms <= STATE_SEND_ACK;
          end
        end
      end
      STATE_SEND_ACK: begin
        if (!scl) begin
          data_counter <= 0;
          highz_mode <= 1;
          wait_scl <= 0;
          state_ms <= STATE_RECEIVE_BYTE;
        end else
        if (byte_counter == DATA_WIDTH / 8 + 1) begin
          highz_mode <= 0;
          stop_transaction <= 1;
          wait_scl <= 0;
          state_ms <= STATE_FINALIZE;
        end else begin
          ready <= 0;
          wait_scl <= 1;
        end
      end
      STATE_FINALIZE: begin
        stop_transaction <= 0;
        state_ms <= STATE_IDLE;
      end
    endcase
  end
end

assign sda = highz_mode ? 1'bz : start_stop || sda_o ;
assign data_out = shiftreg;

endmodule
