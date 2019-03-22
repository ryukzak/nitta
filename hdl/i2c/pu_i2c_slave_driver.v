//////////////////////////////////////////////////////////////////////////////////
// Simple I2C slave controller
//////////////////////////////////////////////////////////////////////////////////

module pu_i2c_slave_driver 
  #( parameter I2C_DATA_WIDTH = 8
   , parameter DATA_WIDTH     = 32
   , parameter ADDRES_DEVICE  = 7'h25
   )
  ( input                           clk
  , input                           rst
  // system interface
  , input      [I2C_DATA_WIDTH-1:0] data_in  
  , output reg [I2C_DATA_WIDTH-1:0] data_out 
  , output reg                      ready_write
  , output reg                      ready_read
  , output reg                      i2c_prepare
  // i2c interface
  , input                           scl
  , inout                           sda
  );

localparam STATE_IDLE           = 0;
localparam STATE_RECEIVE_ADDRES = 1;
localparam STATE_SEND_ACK       = 2;
localparam STATE_SEND_BYTE      = 3;
localparam STATE_RECEIVE_BYTE   = 4;
localparam STATE_FINALIZE       = 5;
localparam STATE_WAIT           = 6;
reg [2:0] state_ms;

localparam STATE_WAIT_SCL_0     = 1;
localparam STATE_WAIT_SCL_1     = 0;
reg       state_scl;

localparam ENABLE               = 1;
localparam DISABLE              = 0;

localparam DATA_COUNTER_WIDTH = $clog2( I2C_DATA_WIDTH + 1 );
reg [DATA_COUNTER_WIDTH-1:0] data_counter; 

localparam BYTE_COUNTER_WIDTH = $clog2( DATA_WIDTH / 8 + 1 );
reg [BYTE_COUNTER_WIDTH-1:0] byte_counter;

reg [I2C_DATA_WIDTH-1:0] shiftreg;
reg sda_en_o;   
reg start_sda_t; 
reg signal_wr;    
reg sda_o;       
reg valid_addr;   

always @(negedge clk) begin
  if ( rst ) begin
    data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
    byte_counter <= {BYTE_COUNTER_WIDTH{1'b0}};
    shiftreg     <= {I2C_DATA_WIDTH{1'b0}};
    sda_o        <= 1'b0;
    valid_addr   <= 1'b0;
    ready_read   <= DISABLE;
    ready_write  <= DISABLE;
    i2c_prepare  <= DISABLE;
    sda_en_o     <= DISABLE;
    start_sda_t  <= DISABLE;
    signal_wr    <= DISABLE;
    state_scl    <= STATE_WAIT_SCL_1;
    state_ms     <= STATE_IDLE;
  end else begin    
    case ( state_ms )
      STATE_IDLE: begin
        if ( ~sda && ~scl ) begin
          data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
          byte_counter <= {BYTE_COUNTER_WIDTH{1'b0}};
          data_out     <= {I2C_DATA_WIDTH{1'b0}};
          shiftreg     <= {I2C_DATA_WIDTH{1'b0}};
          sda_o        <= 0;
          ready_read   <= DISABLE;
          ready_write  <= DISABLE;
          state_scl    <= STATE_WAIT_SCL_1;
          state_ms     <= STATE_RECEIVE_ADDRES;
        end
      end
      STATE_RECEIVE_ADDRES: begin
        case ( state_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              if (data_counter == I2C_DATA_WIDTH) begin
                data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
                sda_o        <= ~(shiftreg[7:1] == ADDRES_DEVICE);
                valid_addr   <= shiftreg[7:1] == ADDRES_DEVICE;
                signal_wr    <= shiftreg[0];
                sda_en_o     <= ENABLE;
                start_sda_t  <= ENABLE;                
                state_ms     <= STATE_SEND_ACK; 
              end 
              state_scl      <= STATE_WAIT_SCL_1;              
            end            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              shiftreg     <= {shiftreg[I2C_DATA_WIDTH - 2:0], sda};
              data_counter <= data_counter + 1;
              state_scl    <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase
      end
      STATE_SEND_ACK: begin 
        ready_write <= DISABLE; 
        case ( state_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              sda_o       <= 1'b0; 
              sda_en_o    <= DISABLE;        
              ready_read  <= signal_wr && start_sda_t;
              if( valid_addr ) begin
                state_ms    <= STATE_WAIT;  
              end else begin
                state_ms    <= STATE_FINALIZE;
              end              
            end
            i2c_prepare <= 1'b0;            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin    
              i2c_prepare <= signal_wr && byte_counter != (DATA_WIDTH / I2C_DATA_WIDTH);         
              state_scl <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase       
      end
      STATE_WAIT: begin
        if (t_start_stop) begin
          ready_read   <= DISABLE;
          ready_write  <= DISABLE;
          state_ms     <= STATE_FINALIZE;
        end else 
        if (signal_wr && start_sda_t) begin
          data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
          shiftreg     <= data_in;
          ready_read   <= DISABLE;
          state_scl    <= STATE_WAIT_SCL_0;
          state_ms     <= STATE_SEND_BYTE;
        end else 
        if (~signal_wr && start_sda_t) begin
          data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
          state_scl    <= STATE_WAIT_SCL_0;
          state_ms     <= STATE_RECEIVE_BYTE;
        end
      end
      STATE_SEND_BYTE: begin
        case ( state_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              sda_en_o     <= ENABLE;                
              sda_o        <= shiftreg[I2C_DATA_WIDTH-1];
              data_counter <= data_counter + 1;
              state_scl    <= STATE_WAIT_SCL_1;
              if (data_counter == I2C_DATA_WIDTH) begin
                if (byte_counter == (DATA_WIDTH / I2C_DATA_WIDTH - 1)) begin
                  start_sda_t <= DISABLE;
                end 
                byte_counter  <= byte_counter + 1;
                sda_en_o      <= DISABLE;           
                state_scl     <= STATE_WAIT_SCL_1;
                state_ms      <= STATE_SEND_ACK;
              end
            end            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              shiftreg  <= {shiftreg[I2C_DATA_WIDTH - 2:0], 1'b0};
              state_scl <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase
      end
      STATE_RECEIVE_BYTE: begin
        case ( state_scl )
          STATE_WAIT_SCL_0: begin
            if (~scl) begin
              data_counter <= data_counter + 1;
              sda_en_o     <= DISABLE;                
              state_scl    <= STATE_WAIT_SCL_1;
              if (data_counter == I2C_DATA_WIDTH) begin 
                if (byte_counter == (DATA_WIDTH / I2C_DATA_WIDTH - 1)) begin
                  start_sda_t <= DISABLE;
                end 
                byte_counter  <= byte_counter + 1;                
                sda_en_o      <= ENABLE;
                ready_write   <= ENABLE;
                data_out      <= shiftreg;
                sda_o         <= 1'b0;
                state_ms      <= STATE_SEND_ACK;
              end
            end            
          end
          STATE_WAIT_SCL_1: begin
            if (scl) begin
              shiftreg  <= {shiftreg[I2C_DATA_WIDTH - 2:0], sda};
              state_scl <= STATE_WAIT_SCL_0;  
            end            
          end
        endcase        
      end
      STATE_FINALIZE: begin 
        data_counter <= {DATA_COUNTER_WIDTH{1'b0}};
        byte_counter <= {BYTE_COUNTER_WIDTH{1'b0}};
        shiftreg     <= {I2C_DATA_WIDTH{1'b0}};
        sda_o        <= 1'b0;
        valid_addr   <= 1'b0;
        ready_read   <= DISABLE;
        ready_write  <= DISABLE;
        sda_en_o     <= DISABLE;
        start_sda_t  <= DISABLE;
        signal_wr    <= DISABLE;
        state_scl    <= STATE_WAIT_SCL_1;
        if(t_start_stop) state_ms <= STATE_IDLE;
      end
    endcase
  end
end

reg curr_sda;
always @(posedge clk) begin
  if (rst) begin
    curr_sda   <= 1'b1;
  end else begin
    curr_sda <= sda;
  end
end

reg t_start_stop;
reg prev_sda;
always @(posedge clk) begin
  if (rst) begin
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

assign sda = sda_en_o ? sda_o : 1'bz;

endmodule