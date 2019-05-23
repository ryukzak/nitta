`timescale 1 ms/ 1 ms
module pu_i2c_master_driver_tb();

reg  clk;
reg  rst;
wire scl;
wire sda;
reg start_transaction;
reg rw;
wire i2c_prepare;
wire i2c_prepare_slave;
reg [7:0] data_in;
reg [7:0] data_in_slave;

localparam READ  = 1;
localparam WRITE = 0;

pu_i2c_slave_driver
  #( .I2C_DATA_WIDTH( 8 )
   , .DATA_WIDTH( 32 )
   , .ADDRES_DEVICE( 7'h47 )
   ) slave_driver_i2c 
  ( .clk( clk )
  , .rst( rst )
  , .scl( scl )
  , .sda( sda )
  , .i2c_prepare( i2c_prepare_slave )
  , .data_in( data_in_slave )
  );

pu_i2c_master_driver
  #( .I2C_DATA_WIDTH( 8 )
   , .DATA_WIDTH( 32 )
   , .ADDRES_DEVICE( 7'h47 )
   ) master_driver_i2c 
  ( .clk( clk )
  , .rst( rst )
  , .scl( scl )
  , .sda( sda )
  , .start_transaction( start_transaction )
  , .rw( rw )
  , .i2c_prepare( i2c_prepare )
  , .data_in( data_in )
  );

always begin
  clk <= 1'b1;
  rst <= 1'b1;
  data_in <= 0;
  start_transaction <= 1'b0;
  repeat(4) #5 clk <= ~clk;
  rst <= 1'b0;
  forever #5 clk <= ~clk;
end

initial begin

  @(negedge rst);

  rw <= WRITE;
  start_transaction <= 1'b1; @(posedge clk);
  start_transaction <= 1'b0; 

  // repeat(1000) @(posedge clk);

  // rw <= WRITE;
  // start_transaction <= 1'b1; @(posedge clk);
  // start_transaction <= 1'b0; 

  repeat(1000) @(posedge clk);

  rw <= READ;
  start_transaction <= 1'b1; @(posedge clk);
  start_transaction <= 1'b0; 
   
  repeat(1000) @(posedge clk); $finish;
end

initial begin

  @(posedge i2c_prepare); @(posedge clk);
  data_in <= 8'hA1;       @(posedge clk);

  @(posedge i2c_prepare); @(posedge clk);
  data_in <= 8'hA2;       @(posedge clk);

  @(posedge i2c_prepare); @(posedge clk);
  data_in <= 8'hA3;       @(posedge clk);

  @(posedge i2c_prepare); @(posedge clk);
  data_in <= 8'hA4;       @(posedge clk);

  // @(posedge i2c_prepare); @(posedge clk);
  // data_in <= 8'hB1;       @(posedge clk);

  // @(posedge i2c_prepare); @(posedge clk);
  // data_in <= 8'hB2;       @(posedge clk);

  // @(posedge i2c_prepare); @(posedge clk);
  // data_in <= 8'hB3;       @(posedge clk);

  // @(posedge i2c_prepare); @(posedge clk);
  // data_in <= 8'hB4;       @(posedge clk);

  @(posedge i2c_prepare_slave); @(posedge clk);
  data_in_slave <= 8'hC1;       @(posedge clk);

  @(posedge i2c_prepare_slave); @(posedge clk);
  data_in_slave <= 8'hC2;       @(posedge clk);

  @(posedge i2c_prepare_slave); @(posedge clk);
  data_in_slave <= 8'hC3;       @(posedge clk);

  @(posedge i2c_prepare_slave); @(posedge clk);
  data_in_slave <= 8'hC4;       @(posedge clk);
end

initial begin
  $dumpfile("pu_i2c_master_driver_tb.vcd");
  $dumpvars(-1, pu_i2c_master_driver_tb);
end 

endmodule
