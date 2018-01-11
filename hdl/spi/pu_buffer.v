module pu_buffer
  #( parameter DATA_WIDTH  = 8
  ,  parameter BUF_SIZE    = 8
  )
  ( input                         clk
  , input                         oe  
  , input                         wr  
  , input                         rst
  , input       [DATA_WIDTH-1:0]  data_in
  , output reg  [DATA_WIDTH-1:0]  data_out
  , output                        buffer_full
  );

localparam ADDR_WIDTH  = $clog2( BUF_SIZE );

reg [DATA_WIDTH-1:0] memory [0:BUF_SIZE-1];
reg [ADDR_WIDTH-1:0]          addres_oe; 
reg [ADDR_WIDTH:0]            addres_wr; // +1 на переполнение
reg addr_incremented_oe;
reg addr_incremented_wr;

always @(posedge clk or posedge rst) begin
  if ( rst ) begin
    addres_oe <= 0;
    addres_wr <= 0;
    addr_incremented_oe <= 0;
    addr_incremented_wr <= 0;
  end
  else begin
    if ( wr ) begin
      memory [ addres_wr ] <= data_in;
      addr_incremented_wr = 1;
      addres_wr <= { 1'b0, addres_wr[ ADDR_WIDTH-1:0 ]}; 
    end else begin 
      if ( addr_incremented_wr ) begin
        addres_wr <= addres_wr + 1'b1;
        addr_incremented_wr = 0;
      end
    end
    if ( oe ) begin
      data_out <= memory [ addres_oe ];
      addr_incremented_oe <= 1;
    end else begin
      if ( addr_incremented_oe ) begin
        addres_oe <= addres_oe + 1'b1;
        addr_incremented_oe = 0;
      end
    end
  end
end

assign buffer_full = { addres_wr [ADDR_WIDTH] };

endmodule