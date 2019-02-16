module i2c_slave_driver 
  #( parameter I2C_DATA_WIDTH = 8
   , parameter DATA_WIDTH     = 32
   , parameter ATTR_WIDTH     = 4
   , parameter ADDRES_DEVICE  = 7'h17
   )
  ( input                   clk
  , input                   rst
  // system interface
  , input  [I2C_DATA_WIDTH-1:0] data_in  
  , output [I2C_DATA_WIDTH-1:0] data_out 
  , output                  ready

  // i2c interface
  , input                   scl
  , inout                   sda
  );

localparam STATE_IDLE           = 0;
localparam STATE_RECEIVE_ADDRES = 1;
localparam STATE_RECEIVE_BYTE   = 2;
localparam STATE_SEND_BYTE      = 3;
localparam STATE_SEND_ACK       = 4;
localparam STATE_RECEIVE_ACK    = 5;
localparam STATE_FINALIZE       = 6;
reg [2:0] state_ms;
reg highz_mode;
reg rw;

reg sda_o;
reg [I2C_DATA_WIDTH-1:0] shiftreg;
reg [I2C_DATA_WIDTH-1:0] shiftreg_send;

localparam BYTE_COUNTER_WIDTH = $clog2( DATA_WIDTH / 8 + 1 );
reg [BYTE_COUNTER_WIDTH-1:0] byte_counter;

localparam DATA_COUNTER_WIDTH = $clog2( I2C_DATA_WIDTH + 1 );
reg [DATA_COUNTER_WIDTH-1:0] data_counter;
reg validation;
reg ready_slave;

always @(posedge rst, posedge clk) begin
  if ( rst ) begin
    sda_o <= 0;
    highz_mode <= 1;
    data_counter <= 0;
    byte_counter <= 0;
    validation <= 0;
    rw <= 0;
    ready_slave <= 0;
    state_ms <= STATE_IDLE;
  end
  else begin
    case ( state_ms )
      STATE_IDLE: begin
        if ( !sda && scl ) begin
          data_counter <= 0;
          byte_counter <= 0;
          state_ms <= STATE_RECEIVE_ADDRES;
        end
      end
      STATE_RECEIVE_ADDRES: begin
        if (scl) begin
          data_counter <= data_counter + 1;
          shiftreg <= {shiftreg[I2C_DATA_WIDTH - 2:0], sda};       
        end else begin
          if ( data_counter == I2C_DATA_WIDTH ) begin
            highz_mode <= 0;
            sda_o <= !(shiftreg[7:1] == ADDRES_DEVICE);
            validation <= !(shiftreg[7:1] == ADDRES_DEVICE);
            rw <= shiftreg[0];
            ready_slave <= shiftreg[0] ? 1 : 0;
            state_ms <= STATE_SEND_ACK; 
          end  
        end
      end
      STATE_SEND_ACK: begin
        if (!scl) begin
          data_counter <= 0;
          highz_mode <= !rw;
          state_ms <= rw ? STATE_SEND_BYTE : STATE_RECEIVE_BYTE;
        end else 
        if (byte_counter == DATA_WIDTH / 8) begin
          highz_mode <= 1;
          state_ms <= STATE_FINALIZE; 
        end else begin
          shiftreg_send <= data_in;
          ready_slave <= 0;
        end
      end
      STATE_RECEIVE_BYTE: begin
        if (scl) begin
          data_counter <= data_counter + 1;
          shiftreg <= {shiftreg[I2C_DATA_WIDTH - 2:0], sda};       
        end else begin
          if ( data_counter == I2C_DATA_WIDTH ) begin
            highz_mode <= 0;
            sda_o <= 0;
            byte_counter <= byte_counter + 1;
            state_ms <= STATE_SEND_ACK; 
          end  
        end
      end
      STATE_SEND_BYTE: begin
        ready_slave <= 0;
        if (!scl) begin          
          shiftreg_send <= {shiftreg_send[I2C_DATA_WIDTH - 2:0], 1'b0};         
          sda_o <= shiftreg_send[I2C_DATA_WIDTH-1];
          data_counter <= data_counter + 1;
          if ( data_counter == I2C_DATA_WIDTH ) begin
            highz_mode <= 1;
            sda_o <= 0;
            byte_counter <= byte_counter + 1;
            state_ms <= STATE_RECEIVE_ACK; 
          end
        end  
      end
      STATE_RECEIVE_ACK: begin
        if (!sda && !scl) begin
          highz_mode <= 0;
          data_counter <= 0;
          shiftreg_send <= data_in;
          ready_slave <= 0;
          state_ms <= rw ? STATE_SEND_BYTE : STATE_RECEIVE_BYTE; 
        end else 
        if (byte_counter == DATA_WIDTH / 8) begin
          highz_mode <= 1;
          sda_o <= 0;
          state_ms <= STATE_FINALIZE; 
        end else begin
          ready_slave <= 1;
        end
      end
      STATE_FINALIZE: begin
        highz_mode <= 1;
        if ( !sda && scl ) state_ms <= STATE_IDLE;
      end
    endcase
  end
end

assign sda = highz_mode ? 1'bz : sda_o;
assign ready = (state_ms == STATE_RECEIVE_BYTE && data_counter == I2C_DATA_WIDTH) || ready_slave;
assign data_out = shiftreg;

endmodule