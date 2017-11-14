module spi_loop (                             
  input clk,
  input rst,
  input [7:0] data_in,
  output reg [7:0] data_out,
  input master_slave_output_select,

  input master_spi_miso,
  output master_spi_mosi,
  output master_spi_sclk,
  output master_spi_cs,

  output slave_spi_miso,
  input slave_spi_mosi,
  input slave_spi_sclk,
  input slave_spi_cs
  );  

parameter TIMER = 18;
reg [7:0] send_timer;

wire start = (!rst && send_timer == 0) ? 1 : 0;

always @(posedge clk)
  begin
    if (rst || send_timer == TIMER)
      send_timer <= 0;
    else
      send_timer <= send_timer + 1;
  end
  
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
wire slave_ready;
wire [7:0] slave_out;

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
  if ( slave_ready ) slave_in <= { slave_out[0], slave_out[1], slave_out[2], slave_out[3], slave_out[4], slave_out[5], slave_out[6], slave_out[7] };
  // if ( slave_ready ) slave_in <= slave_out;

always @(posedge clk) 
  if (master_slave_output_select && master_ready)
    data_out <= master_out;
  else if (!master_slave_output_select && slave_ready) 
    data_out <= slave_out;
	
endmodule
