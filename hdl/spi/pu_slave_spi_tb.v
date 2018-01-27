`timescale 1 ms/ 1 ms
module pu_slave_spi_tb
  #( parameter DATA_WIDTH      = 32
  ,  parameter SPI_DATA_WIDTH  = 8
  ,  parameter SCLK_HALFPERIOD = 1
  ,  parameter BUF_SIZE        = 6
  )
  ();

reg clk;
reg rst;
reg start_transaction;
reg  [SPI_DATA_WIDTH-1:0] master_in;
wire [SPI_DATA_WIDTH-1:0] master_out;
reg  [DATA_WIDTH-1:0] slave_in;
wire [DATA_WIDTH-1:0] slave_out;

wire mosi;
wire miso;
wire sclk;
wire cs;

spi_master_driver master 
  ( .clk(clk)
  , .rst(rst)
  , .start_transaction(start_transaction)
  , .data_in(master_in)
  , .data_out(master_out)
  , .ready(ready)
  , .miso( miso )
  , .mosi( mosi )
  , .sclk( sclk )
  , .cs( cs )
  );

pu_slave_spi #( .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
              , .BUF_SIZE( BUF_SIZE )
   
              ) pu 
  ( .clk( clk )
  , .rst( rst )
  // , .signal_cycle( cycle )

  //, .signal_wr( wr )
  //, .data_in(slave_in)
  //, .attr_in(slave_in)

  //, .signal_oe( oe )
  , .data_out( slave_out )
  //, .attr_out(slave_in)

  // , .flag_start( start )
  , .flag_stop( ready )

  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );

reg [4:0] i;

always begin
  #5 clk = ~clk;
end 

initial 
  begin
    $display("Start");
    clk = 0;                     @(posedge clk);
    rst = 1;                     @(posedge clk);
    rst = 0;                     @(posedge clk);

    master_in = 8'h11; slave_in = 8'h55; @(posedge clk);
    start_transaction = 1;               @(posedge clk);
    start_transaction = 0;               @(posedge clk);

    repeat(18) @(posedge clk);

    master_in = 8'h22; slave_in = 8'h33; @(posedge clk);
    start_transaction = 1;               @(posedge clk);
    start_transaction = 0;               @(posedge clk);

    repeat(18) @(posedge clk);

    master_in = 8'h33; slave_in = 8'h33; @(posedge clk);
    start_transaction = 1;               @(posedge clk);
    start_transaction = 0;               @(posedge clk);

    repeat(18) @(posedge clk);

    master_in = 8'h44; slave_in = 8'h33; @(posedge clk);
    start_transaction = 1;               @(posedge clk);
    start_transaction = 0;               @(posedge clk);

    repeat(35) @(posedge clk); 
  
    $display("Buffer dump by a words:");
  	for ( i = 0; i < BUF_SIZE; i = i + 1 )
	    begin
        $display("%d -> %h", i, pu.buffer.memory[i]);
    	end

    $finish;

  end


initial
  begin
    $dumpfile("pu_slave_spi_tb.vcd");
    $dumpvars(0, pu_slave_spi_tb);
    $display("finish");
  end 

endmodule