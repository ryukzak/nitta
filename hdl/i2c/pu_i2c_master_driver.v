//////////////////////////////////////////////////////////////////////////////////
// Simple I2C master controller
//////////////////////////////////////////////////////////////////////////////////

module pu_i2c_master_driver
  #( parameter I2C_DATA_WIDTH = 8
   , parameter DATA_WIDTH     = 32
   , parameter ADDRES_DEVICE  = 7'h25
   , parameter SCL_HALFPERIOD = 8
   )
  ( input                           clk
  , input                           rst
  , input                           start_transaction
  , input                           rw
  // system interface
  , input      [I2C_DATA_WIDTH-1:0] data_in
  , output reg                      i2c_prepare

  , output reg [I2C_DATA_WIDTH-1:0] data_out
  , output reg                      ready_write

  // i2c interface
  , output reg                      scl
  , inout                           sda
  );

localparam STATE_IDLE     = 0;
localparam STATE_SCL_LOW  = 1;
localparam STATE_SCL_HIGH = 2;
reg [1:0] state_scl;
reg scl_start;
reg scl_stop;

localparam SCL_COUNTER_WIDTH = $clog2(SCL_HALFPERIOD);
reg   [SCL_COUNTER_WIDTH-1:0] scl_count;

always @( posedge rst, negedge clk ) begin
  if (rst || scl_stop) begin
    scl       <= 1'b1;
    state_scl <= STATE_IDLE;
  end
  else begin
    case ( state_scl )
      STATE_IDLE: begin
        scl <= 1'b1;
        if ( scl_start ) begin
          state_scl <= STATE_SCL_LOW;
        end
      end
      STATE_SCL_LOW: begin
        scl <= 1'b0;
        if (!scl_count) begin
          state_scl <= STATE_SCL_HIGH;
        end
      end
      STATE_SCL_HIGH: begin
        scl <= 1'b1;
        if (!scl_count) begin
          state_scl <= STATE_SCL_LOW;
        end
      end
    endcase
  end
end

always @(posedge clk) begin
  if ( scl_count && !start_transaction ) scl_count <= scl_count - 1;
  else                                   scl_count <= SCL_HALFPERIOD - 1;
end

localparam STATE_SEND_ADDRES  = 1;
localparam STATE_RECEIVE_ACK  = 2;
localparam STATE_SEND_ACK     = 3;
localparam STATE_RECEIVE_BYTE = 4;
localparam STATE_SEND_BYTE    = 5;
localparam STATE_FINALIZE     = 6;
reg [2:0] state_ms;

localparam STATE_WAIT_SCL_0   = 1;
localparam STATE_WAIT_SCL_1   = 0;
reg       wait_scl;
reg       sda_en_o;
reg       sda_o;

localparam ENABLE             = 1;
localparam DISABLE            = 0;

localparam DATA_COUNTER_WIDTH = $clog2( I2C_DATA_WIDTH + 1 );
reg [DATA_COUNTER_WIDTH-1:0] data_counter;

reg [7:0] shiftreg;

localparam BYTE_COUNTER_WIDTH = $clog2( DATA_WIDTH / 8 + 1 );
reg [BYTE_COUNTER_WIDTH-1:0] byte_counter;

always @(posedge rst, negedge clk) begin
  if (rst) begin
    data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
    byte_counter <= {BYTE_COUNTER_WIDTH{1'b0}};
    sda_o        <= 1'b1;
    sda_en_o     <= 1'b1;
    i2c_prepare  <= 1'b0;
    ready_write <= 1'b0;
    scl_start    <= DISABLE;
    scl_stop     <= DISABLE;
    shiftreg    <= {ADDRES_DEVICE, rw};
    wait_scl     <= STATE_WAIT_SCL_0;
    state_ms     <= STATE_IDLE;
  end else begin
    case(state_ms)
      STATE_IDLE: begin
        scl_stop <= DISABLE;
        sda_o    <= 1'b1;
        if (start_transaction) begin
          data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
          byte_counter <= {BYTE_COUNTER_WIDTH{1'b0}};
          shiftreg <= {ADDRES_DEVICE, rw};
          sda_o     <= 1'b0;
          scl_start <= ENABLE;
          sda_en_o  <= ENABLE;
          wait_scl  <= STATE_WAIT_SCL_0;
          state_ms  <= STATE_SEND_ADDRES;
        end
      end
      STATE_SEND_ADDRES: begin
        scl_start <= DISABLE;
        case ( wait_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              if (data_counter == I2C_DATA_WIDTH) begin
                state_ms <= STATE_RECEIVE_ACK;
              end
              sda_o        <= shiftreg[7];
              wait_scl     <= STATE_WAIT_SCL_1;
            end
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              shiftreg <= {shiftreg[6:0], 1'b0};
              data_counter <= data_counter + 1;
              wait_scl  <= STATE_WAIT_SCL_0;
            end
          end
        endcase
      end
      STATE_RECEIVE_ACK: begin
        sda_en_o <= DISABLE;
        if (scl) begin
          if (sda || byte_counter == (DATA_WIDTH / I2C_DATA_WIDTH)) begin
            wait_scl <= STATE_WAIT_SCL_0;
            state_ms <= STATE_FINALIZE;
          end else begin
            if (rw) begin
              wait_scl     <= STATE_WAIT_SCL_0;
              state_ms     <= STATE_RECEIVE_BYTE;
            end else begin
              i2c_prepare  <= 1'b1;
              wait_scl     <= STATE_WAIT_SCL_1;
              state_ms     <= STATE_SEND_BYTE;
            end
            data_counter   <= 0;
            //wait_scl       <= STATE_WAIT_SCL_1;
          end
        end
      end
      STATE_SEND_BYTE: begin
        i2c_prepare <= 1'b0;
        case ( wait_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              sda_en_o     <= ENABLE;
              sda_o        <= shiftreg[I2C_DATA_WIDTH-1];
              data_counter <= data_counter + 1;
              wait_scl     <= STATE_WAIT_SCL_1;
              if (data_counter == I2C_DATA_WIDTH) begin
                sda_en_o   <= DISABLE;
                byte_counter <= byte_counter + 1;
                state_ms   <= STATE_RECEIVE_ACK;
              end
            end
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              if (!data_counter) shiftreg <= data_in;
              else               shiftreg <= {shiftreg[I2C_DATA_WIDTH - 2:0], 1'b0};
              wait_scl <= STATE_WAIT_SCL_0;
            end
          end
        endcase
      end
      STATE_RECEIVE_BYTE: begin
        case ( wait_scl )
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              shiftreg  <= {shiftreg[I2C_DATA_WIDTH - 2:0], sda};
              data_counter <= data_counter + 1;
              wait_scl  <= STATE_WAIT_SCL_0;
            end
          end
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              if (data_counter == I2C_DATA_WIDTH) begin
                sda_en_o <= ENABLE;
                byte_counter <= byte_counter + 1;
                data_counter <= 0;
                sda_o    <= 1'b0;
                ready_write <= 1'b1;
                data_out <= shiftreg;
                state_ms <= STATE_SEND_ACK;
              end
              wait_scl <= STATE_WAIT_SCL_1;
            end
          end
        endcase
      end
      STATE_SEND_ACK: begin
        ready_write <= 1'b0;
        case ( wait_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              sda_en_o <= DISABLE;
              if (byte_counter == (DATA_WIDTH / I2C_DATA_WIDTH)) begin
                wait_scl <= STATE_WAIT_SCL_0;
                state_ms <= STATE_FINALIZE;
              end else begin
                state_ms <= STATE_RECEIVE_BYTE;
              end
            end
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              wait_scl <= STATE_WAIT_SCL_0;
            end
          end
        endcase
      end
      STATE_FINALIZE: begin
        case ( wait_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              sda_en_o <= ENABLE;
              wait_scl <= STATE_WAIT_SCL_1;
            end
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              scl_stop <= ENABLE;
              state_ms <= STATE_IDLE;
            end
          end
        endcase
      end
    endcase
  end
end

assign sda = sda_en_o ? sda_o : 1'bz;

endmodule
