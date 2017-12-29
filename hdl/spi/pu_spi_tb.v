`timescale 1 ms/ 1 ms
module pu_spi_tb
#(  
    parameter DATA_WIDTH      = 8
,   parameter SCLK_HALFPERIOD = 1
)
();

reg clk;
reg rst;
reg start_transaction;
reg  [DATA_WIDTH-1:0] master_in;
wire [DATA_WIDTH-1:0] master_out;
reg  [DATA_WIDTH-1:0] slave_in;
wire [DATA_WIDTH-1:0] slave_out;

wire master_spi_miso;
wire master_spi_mosi;
wire master_spi_sclk;
wire master_spi_cs;

wire slave_spi_miso;
wire slave_spi_mosi;
wire slave_spi_sclk;
wire slave_spi_cs;

assign master_spi_miso = slave_spi_miso;
assign slave_spi_mosi = master_spi_mosi;
assign slave_spi_sclk = master_spi_sclk;
assign slave_spi_cs   = master_spi_cs;

spi_master_driver master (
  .clk(clk)
, .rst(rst)
, .start_transaction(start_transaction)
, .data_in(master_in)
, .data_out(master_out)
, .ready(ready)
, .miso(master_spi_miso)
, .mosi(master_spi_mosi)
, .sclk(master_spi_sclk)
, .cs(master_spi_cs)
);

pu_spi slave (
  .clk(clk)
, .rst(rst)
//, .data_in(slave_in)
, .data_out(slave_out)
, .ready(ready)
, .miso(slave_spi_miso)
, .mosi(slave_spi_mosi)
, .sclk(slave_spi_sclk)
, .cs(slave_spi_cs)
);

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

    repeat(35) @(posedge clk); $finish;

  end


initial
  begin
    $dumpfile("pu_spi_tb.vcd");
    $dumpvars(-1, pu_spi_tb);
    $display("finish");
  end 

endmodule