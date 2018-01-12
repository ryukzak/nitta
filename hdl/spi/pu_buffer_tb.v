`timescale 1 ms/ 1 ms
module pu_buffer_tb
  #( parameter DATA_WIDTH       = 8
  ,  parameter ATTR_WIDTH       = 4
  ,  parameter VALID            = 1
  ,  parameter BUFFER_SIZE      = 3
   )
();

reg  clk;
reg  rst;

reg  [13:0] signals_out;
reg  [DATA_WIDTH-1:0] data_bus;
wire [DATA_WIDTH-1:0] spi_data_out;
reg  [ATTR_WIDTH-1:0] attr_bus;
wire [ATTR_WIDTH-1:0] spi_attr_out;

reg  nitta_cycle;
wire spi_start;
wire spi_stop;

wire spi_mosi;
wire spi_miso;
reg  spi_sclk;
reg  spi_cs;

pu_spi pu_spi_test(

  .clk(clk)
,	.rst(rst)

,	.signal_wr( signals_out[ 12 ] )
,	.data_in( data_bus )
, .attr_in( attr_bus )

,	.signal_oe( signals_out[ 13 ] )
, .data_out( spi_data_out )
, .attr_out( spi_attr_out )

, .flag_cycle( nitta_cycle )
, .flag_start( spi_start )
, .flag_stop( spi_stop )

, .mosi( spi_mosi )
, .miso( spi_miso )
, .sclk( spi_sclk )
, .cs( spi_cs )

);

always begin
  #5 clk = ~clk;
end 

task Nop;
    begin
       signals_out[ 12 ] <= 0;
       signals_out[ 13 ] <= 0;
       nitta_cycle <= 0; 
    end
endtask

task Send;
    input [DATA_WIDTH-1:0] data;
    begin
      data_bus <= data;
      signals_out[ 12 ] <= 1;
      signals_out[ 13 ] <= 0;
    end
endtask

task Receive;
    begin
      signals_out[ 13 ] <= 1;  
      signals_out[ 12 ] <= 0;
    end
endtask

initial 
  begin
  $display("Start");

  clk = 0;       @(posedge clk); 
  rst = 1;       @(posedge clk); 
  rst = 0;       @(posedge clk);
  Nop();			   @(posedge clk);

  Send(1);   @(posedge clk); @(posedge clk);
  Send(2);   @(posedge clk); @(posedge clk);

  Nop();			   @(posedge clk);

  Receive(); @(posedge clk); @(posedge clk);
  Send(3);   @(posedge clk); @(posedge clk);
  Receive(); @(posedge clk); @(posedge clk); 
  
  // Send(1);   @(posedge clk); @(posedge clk);
  // Nop();	   @(posedge clk);
  // Send(2);   @(posedge clk); @(posedge clk);
  // Nop();	   @(posedge clk);
  // Send(3);   @(posedge clk); @(posedge clk);
  // Nop();	   @(posedge clk);
  // Send(4);   @(posedge clk); @(posedge clk);
  // Nop();	   @(posedge clk);

  // Receive(); @(posedge clk); @(posedge clk); 
  // Nop();	   @(posedge clk);
  // Receive(); @(posedge clk); @(posedge clk);
  // Nop();		 @(posedge clk);
  // Receive(); @(posedge clk); @(posedge clk);
  // Nop();		 @(posedge clk);
  // Receive(); @(posedge clk); @(posedge clk);

  // [!] Reset Buffers / New computational cycle
  nitta_cycle <= 1;	@(posedge clk);  
  nitta_cycle <= 0;	@(posedge clk); 

  repeat(10) @(posedge clk); $finish;
  end

initial
  begin
  $dumpfile("pu_spi_tb.vcd");
  $dumpvars(-1, pu_buffer_tb);
  $display("finish");
  end 

endmodule