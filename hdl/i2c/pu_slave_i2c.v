module pu_slave_i2c 
  #( parameter I2C_DATA_WIDTH = 8
   , parameter DATA_WIDTH     = 8
   , parameter ADDRES_DEVICE  = 7'h47
   )
  ( input                   clk
  , input                   rst

  // i2c interface
  , input                   scl
  , inout                   sda  
  );

localparam STATE_IDLE           = 0;
localparam STATE_RECEIVE_ADDRES = 1;
localparam STATE_SEND_ACK       = 2;
localparam STATE_FINALIZE       = 3;
reg [2:0] state_ms;

localparam STATE_WAIT_SCL_0     = 0;
localparam STATE_WAIT_SCL_1     = 1;
reg       state_scl;

reg [I2C_DATA_WIDTH-1:0] shiftreg;
reg [3:0] data_counter;
reg sda_o;
reg start_sent_sdo;

always @(negedge rst, negedge clk) begin
  if ( ~rst ) begin
    data_counter   <= 0;
    sda_o          <= 0;
    start_sent_sdo <= 0;
    state_scl <= STATE_WAIT_SCL_1;
    state_ms  <= STATE_IDLE;
  end else begin 		
    case ( state_ms )
      STATE_IDLE: begin
        if ( !sda && !scl ) begin
          data_counter <= 0;
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
                sda_o <= shiftreg[7:1] == ADDRES_DEVICE;
                start_sent_sdo <= 1'b1;
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
              start_sent_sdo <= 1'b0;
              state_scl <= STATE_WAIT_SCL_1;  
            end            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              state_scl <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase
        if (t_start_stop) begin
          state_ms <= STATE_FINALIZE;
        end
      end
      STATE_FINALIZE: begin 
        start_sent_sdo <= 1'b0;
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

assign sda = start_sent_sdo ? ~sda_o : 1'bz;

endmodule