module pu_slave_i2c 
  #( parameter I2C_DATA_WIDTH = 8
   , parameter DATA_WIDTH     = 32
   , parameter ADDRES_DEVICE  = 7'h47
   )
  ( input                   clk
  , input                   rst

  // i2c interface
  , input                   scl
  , inout                   sda
    
  , output                  D0
  , output                  D1
  , output                  D2
  , output                  D3
  , output                  D4
  , output                  D5
  , output                  D6
  , output                  D7
  
  );

localparam STATE_IDLE           = 0;
localparam STATE_RECEIVE_ADDRES = 1;
localparam STATE_SEND_ACK       = 2;
localparam STATE_SEND_BYTE      = 3;
localparam STATE_FINALIZE       = 4;
localparam STATE_WAIT           = 5;
reg [2:0] state_ms;

localparam STATE_WAIT_SCL_0     = 0;
localparam STATE_WAIT_SCL_1     = 1;
reg       state_scl;

localparam DATA_COUNTER_WIDTH = $clog2( I2C_DATA_WIDTH + 1 );
reg [DATA_COUNTER_WIDTH:0] data_counter; 

localparam BYTE_COUNTER_WIDTH = $clog2( DATA_WIDTH / 8 + 1 );
reg [BYTE_COUNTER_WIDTH:0] byte_counter;

reg [I2C_DATA_WIDTH-1:0] shiftreg;
reg start_sent_sda;
reg  stop_sent_sda;
reg signal_wr;
reg sda_o;
reg flag;

always @(negedge rst, negedge clk) begin
  if ( ~rst ) begin
    data_counter   <= 0;
    byte_counter   <= 0;
    sda_o          <= 0;
    start_sent_sda <= 0;
    stop_sent_sda  <= 0;
    signal_wr      <= 0;
    flag           <= 1;
    state_scl <= STATE_WAIT_SCL_1;
    state_ms  <= STATE_IDLE;
  end else begin 		
    case ( state_ms )
      STATE_IDLE: begin
        if ( !sda && !scl ) begin
          data_counter <= 0;
          byte_counter <= 0;
          shiftreg     <= 0;
          sda_o        <= 0;
          state_scl <= STATE_WAIT_SCL_1;
          state_ms  <= STATE_RECEIVE_ADDRES;
        end
      end
      STATE_RECEIVE_ADDRES: begin
        case ( state_scl )
          STATE_WAIT_SCL_0: begin
            if (!scl) begin
              if (data_counter == I2C_DATA_WIDTH) begin
                state_scl <= STATE_WAIT_SCL_1;
                sda_o <= ~(shiftreg[7:1] == ADDRES_DEVICE);
                signal_wr <= shiftreg[0];
                start_sent_sda <= 1'b1;
                stop_sent_sda <= 1;
                state_ms <= STATE_SEND_ACK; 
              end else begin
                state_scl <= STATE_WAIT_SCL_1;  
              end              
            end            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              shiftreg <= {shiftreg[I2C_DATA_WIDTH - 2:0], sda};
              data_counter <= data_counter + 1;
              state_scl <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase
      end
      STATE_SEND_ACK: begin  

        case ( state_scl )
          STATE_WAIT_SCL_0: begin
            if (!scl) begin
              sda_o <= 1'b0;
              start_sent_sda <= 1'b0;
              state_ms <= STATE_WAIT;  
            end            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              state_scl <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase
       
      end
      STATE_WAIT: begin
        if (t_start_stop) begin
          state_ms <= STATE_FINALIZE;
        end else 
        if (signal_wr && stop_sent_sda) begin
          shiftreg <= 8'h8F;
          data_counter <= 0;
          state_scl <= STATE_WAIT_SCL_0;
          state_ms <= STATE_SEND_BYTE;
        end
      end
      STATE_SEND_BYTE: begin

        case ( state_scl )
          STATE_WAIT_SCL_0: begin
            if (!scl) begin
              start_sent_sda <= 1;                
              sda_o <= shiftreg[I2C_DATA_WIDTH-1];
              data_counter <= data_counter + 1;
              state_scl <= STATE_WAIT_SCL_1;
              if (data_counter == I2C_DATA_WIDTH) begin
                start_sent_sda <= 0;
                
                if (byte_counter == DATA_WIDTH / I2C_DATA_WIDTH - 1) begin
                  stop_sent_sda <= 0;
                end else begin
                  byte_counter <= byte_counter + 1;  
                end

                state_ms <= STATE_SEND_ACK;
              end
            end            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              shiftreg <= {shiftreg[I2C_DATA_WIDTH - 2:0], 1'b0};
              state_scl <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase

      end
      STATE_FINALIZE: begin 
        start_sent_sda <= 1'b0;
        state_ms <= STATE_IDLE;
      end
    endcase
  end
end

// Получаем текущее значение sda и сохраняем его.
// Нужно для нахождения начала и конца передачи.
reg curr_sda;
always @(negedge rst, posedge clk) begin
  if (~rst) begin
    curr_sda   <= 1'b1;
  end else begin
    curr_sda <= sda;
  end
end

// Определение начала и конца передачи через
// предыдущее значение curr_sda. 
reg t_start_stop;
reg prev_sda;
always @(negedge rst, posedge clk) begin
  if (~rst) begin
    t_start_stop <= 1'b0;
    prev_sda <= curr_sda;
  end else begin
    if (scl) begin
      if (prev_sda != sda) begin
        t_start_stop <= 1'b1;
        prev_sda <= curr_sda;
      end else begin
        t_start_stop <= 1'b0;
      end
    end else begin
      prev_sda <= curr_sda;  
    end    
  end
end

assign sda = start_sent_sda ? sda_o : 1'bz;

assign D0 = sda;
assign D1 = scl;
assign D2 = sda_o;
assign D3 = start_sent_sda;
assign D4 = stop_sent_sda;
assign D5 = flag;
assign D6 = t_start_stop;

endmodule