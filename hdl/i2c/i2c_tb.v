`timescale 1 ms/ 1 ms

module i2c_tb
  #( parameter DATA_WIDTH      = 32
   , parameter ATTR_WIDTH      = 4
   , parameter I2C_DATA_WIDTH  = 8
   , parameter ADDRES_DEVICE   = 7'h47
   )
  ();

reg clk;
reg rst;
reg start_transaction;

reg scl;
wire sda;

reg highz_mode;
reg sda_o;

assign sda = sda_o;

localparam READ  = 0;
localparam WRITE = 1;

pu_slave_i2c #
  ( .DATA_WIDTH( DATA_WIDTH )
  , .ATTR_WIDTH( ATTR_WIDTH )
  , .I2C_DATA_WIDTH( I2C_DATA_WIDTH )
  , .ADDRES_DEVICE( ADDRES_DEVICE )
  ) driver_slave
  ( .clk( clk )
  , .rst( rst )
  , .scl( scl )
  , .sda( sda )
  );

task delay;
  begin
      repeat(10) @(posedge clk);
  end
endtask

initial begin
  clk   <= 0;
  sda_o <= 1;
  scl   <= 1;
  rst   <= 0; @(posedge clk);
  rst   <= 1;
end

always begin
  #5 clk <= ~clk;
end

initial begin
  @(negedge rst); repeat(10) @(posedge clk);

  // Start
  sda_o <= 0; delay(); scl <= 0;

  delay();

  // Send addres: ‭100 0111‬ + rw: 1‬

  // Addres
  sda_o <= 1; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  sda_o <= 0; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  sda_o <= 0; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  sda_o <= 0; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  sda_o <= 1; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  sda_o <= 1; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  sda_o <= 1; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  // rw
  sda_o <= 1; delay(); scl <= 1;
  delay(); sda_o <= 0; scl <= 0;

  // byte 1
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  // ack
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  // byte 2
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  // ack
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  // byte 3
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  // ack
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  // byte 4
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'bz; scl <= 0;

  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  // ack
  sda_o <= 1'bz; delay(); scl <= 1;
  delay(); sda_o <= 1'b0; scl <= 0;

  sda_o <= 0; delay(); scl <= 1;
  delay(); sda_o <= 1; scl <= 1;


  repeat(100) @(posedge clk); $finish;
end

initial begin
  $dumpfile("i2c_tb.vcd");
  $dumpvars(0, i2c_tb);
end

endmodule
