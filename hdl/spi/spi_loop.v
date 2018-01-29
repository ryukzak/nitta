module spi_loop 
  #( parameter TIMER = 18
   )
  ( input clk
  , input rst
  , input select // 1 - reverse, 0 - direct
  , input [7:0] data_in
  , output reg [7:0] data_out
  
  , input master_spi_miso
  , output master_spi_mosi
  , output master_spi_sclk
  , output master_spi_cs

  , output slave_spi_miso
  , input slave_spi_mosi
  , input slave_spi_sclk
  , input slave_spi_cs
  );

localparam SEND_TIMER_WIDTH = $clog2(TIMER);

reg [SEND_TIMER_WIDTH-1:0] send_timer;
always @( posedge clk )
  if ( rst || !send_timer ) send_timer <= TIMER - 1;
  else                      send_timer <= send_timer - 1;

    
wire start = !rst && !send_timer;


wire master_ready;
wire [7:0] master_out;

spi_master_driver master (
  .clk(clk),
  .rst(rst),
  
  .start_transaction(start),
  .data_in(data_in),
  .data_out(master_out),
  .ready(master_ready),
  
  .mosi(master_spi_mosi),
  .miso(master_spi_miso),
  .sclk(master_spi_sclk),
  .cs(master_spi_cs)
);


reg [7:0] slave_in;
wire [7:0] slave_out;
wire slave_ready;

spi_slave_driver slave (
  .clk(clk),
  .rst(rst),
  
  .data_in(slave_in),
  .ready(slave_ready),
  .data_out(slave_out),
  
  .miso(slave_spi_miso),
  .mosi(slave_spi_mosi),
  .sclk(slave_spi_sclk),
  .cs(slave_spi_cs)
);

always @(posedge clk) 
  if ( slave_ready ) begin
    if ( select ) slave_in <= { slave_out[0], slave_out[1], slave_out[2], slave_out[3], slave_out[4], slave_out[5], slave_out[6], slave_out[7] };
    else slave_in <= slave_out;
  end
  
always @(posedge clk) 
  if ( master_ready ) data_out <= master_out;

endmodule
