module pu_fifo
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter FIFO_SIZE = 3
   , parameter ADDR_WIDTH = $clog2( FIFO_SIZE )    
   )
   ( input wire                     clk
   , input wire                     rst
   , input wire  [DATA_WIDTH-1:0]   data_in
   , input wire  [ATTR_WIDTH-1:0]   attr_in
   , input wire                     signal_wr
   , input wire                     signal_oe
   , output wire [DATA_WIDTH-1:0]   data_out
   , output wire [ATTR_WIDTH-1:0]   attr_out
   );

reg [DATA_WIDTH + ATTR_WIDTH - 1:0] fifo_buf [0:FIFO_SIZE-1];            
reg [ADDR_WIDTH-1:0]                fifo_buf_read;    
reg [ADDR_WIDTH-1:0]                fifo_buf_write;         

always @(posedge clk)
begin
  if (rst == 1) begin
  fifo_buf_write <= 0;
  fifo_buf_read <= 0; 
  end  
  if (signal_wr == 1) begin
    fifo_buf[fifo_buf_write] <= { attr_in, data_in };    
    fifo_buf_write <= fifo_buf_write == FIFO_SIZE - 1 ? 0 : fifo_buf_write + 1;
  end else if (signal_oe == 1) begin    
    fifo_buf_read <= fifo_buf_read == FIFO_SIZE - 1 ? 0 : fifo_buf_read + 1;    
  end  
end
assign { attr_out, data_out } = signal_oe ? fifo_buf[fifo_buf_read] : 0; 
endmodule