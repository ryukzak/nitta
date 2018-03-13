module pu_fifo
#(
    parameter DATA_WIDTH = 32,
    parameter ATTR_WIDTH = 4,
    parameter FIFO_SIZE = 3
)
(
    input wire clk,
    input wire rst,
    input wire [DATA_WIDTH-1:0] data_in,
    input wire [ATTR_WIDTH-1:0] attr_in,
    input wire signal_wr,
    input wire signal_oe,
    output reg [DATA_WIDTH-1:0] data_out,
    output reg [ATTR_WIDTH-1:0] attr_out
);
reg [DATA_WIDTH-1+ATTR_WIDTH-1:0] fifo_buf [0:FIFO_SIZE-1];            
reg [FIFO_SIZE-1:0]  fifo_buf_start;    
reg [FIFO_SIZE-1:0]  fifo_buf_end;
reg signal_oe_prev = 0;
reg signal_wr_prev = 0;

always @(posedge clk or posedge rst)
begin  
  if ((signal_wr_prev == 0) && (signal_wr == 1)) begin
    fifo_buf[fifo_buf_end] <= data_in;
    fifo_buf_end <= fifo_buf_end + 1;
  end
  if ((signal_oe_prev == 0) && (signal_oe == 1)) begin
    data_out <= fifo_buf[fifo_buf_start];
    fifo_buf_start <= fifo_buf_start + 1;
  end
  signal_wr_prev <= signal_wr;
  signal_oe_prev <= signal_oe;
end
endmodule