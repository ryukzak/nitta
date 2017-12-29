module pu_buffer 
  #(  
    parameter DATA_WIDTH  = 8
  , parameter BUF_SIZE    = 6 
  )
  ( 
    input                         clk  
  , input                         ready
  , input                         rst
  , input       [DATA_WIDTH-1:0]  data_in
  , output reg  [DATA_WIDTH-1:0]  data_out
  );

reg [DATA_WIDTH-1:0] memory [0:2**BUF_SIZE-1]; 
reg [DATA_WIDTH-1:0]        time_buffer;
reg [BUF_SIZE-1:0]          addres;
reg [2:0]                   state;
reg addr_incremented;

localparam STATE_START_READY  = 0; 
localparam STATE_STOP_READY   = 1; 

always @(posedge clk or posedge rst) begin
  if ( rst ) begin
    addres <= 0;
    addr_incremented <= 0;
    state  <= STATE_START_READY;
  end
  else begin
    if ( ready ) begin
      case (state)
        STATE_START_READY:  begin         
          time_buffer <= memory [ addres ]; // Save in the first line
          memory [ addres + 1 ] <= data_in;	// Write in the second line
          state <= STATE_STOP_READY;
        end
        STATE_STOP_READY:   begin
          data_out <= time_buffer;
          state <= STATE_START_READY;
        end 
      endcase     
    end
    if ( ready ) begin
      if ( !addr_incremented ) begin
        addr_incremented <= 1;
        addres <= addres + 1;
      end
    end else begin
      addr_incremented <= 0;
    end
  end
end

endmodule