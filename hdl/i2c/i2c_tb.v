`timescale 1 ms/ 1 ms

module i2c_tb
  #( parameter DATA_WIDTH      = 32
   , parameter ATTR_WIDTH      = 4
   , parameter I2C_DATA_WIDTH  = 8
   )
  ();

reg clk; 
reg rst; 
reg rw; 
reg start_transaction;

wire scl;
wire sda;
wire ready_master;
wire ready_slave;

reg  [I2C_DATA_WIDTH-1:0] data_in_master;
reg  [I2C_DATA_WIDTH-1:0] data_in_slave;
wire [I2C_DATA_WIDTH-1:0] data_out_master;
wire [I2C_DATA_WIDTH-1:0] data_out_slave;

i2c_master_driver #
  ( .DATA_WIDTH( DATA_WIDTH )
  , .ATTR_WIDTH( ATTR_WIDTH )
  ) driver_master
  ( .clk( clk )
  , .rst( rst )
  , .rw( rw )
  , .ready( ready_master )
  , .data_in( data_in_master )
  , .data_out( data_out_master )
  , .start_transaction( start_transaction )
  , .scl( scl )
  , .sda( sda )
  );

i2c_slave_driver #
  ( .DATA_WIDTH( DATA_WIDTH )
  , .ATTR_WIDTH( ATTR_WIDTH )
  ) driver_slave
  ( .clk( clk )
  , .rst( rst )
  , .data_in( data_in_slave )
  , .data_out( data_out_slave )
  , .ready( ready_slave )
  , .scl( scl )
  , .sda( sda )
  );

always begin
  #5 clk <= ~clk;
end

task assert;
  input [7:0] a;
  input [7:0] b;
  begin
    if ( !(a === b) ) begin
      $display("ASSERT FAIL: %h === %h", a, b);
      $finish(2);
    end
  end
endtask 

initial begin
  clk <= 0;
  rst <= 1;
  rw <= 0;
  start_transaction <= 0; 
  @(posedge clk);
  rst <= 0;
end

initial begin  
  @(negedge rst); 
  repeat(3) @(posedge clk);

  // MASTER (Write) -> SLAVE (Read) 
  rw <= 0;
  start_transaction <= 1; @(posedge clk);
  start_transaction <= 0;
  repeat(120) @(posedge clk); 

  // MASTER (Read) -> SLAVE (Write) 
  rw <= 1;
  start_transaction <= 1; @(posedge clk);
  start_transaction <= 0;
  repeat(120) @(posedge clk);

  repeat(100) @(posedge clk); $finish;
end

initial begin  
  @(negedge rst); 

  // Пишим слово в SLAVE 32'hA1B1C1D1
  @(posedge start_transaction);
  @(posedge ready_master); data_in_master <= 8'hA1;
  @(posedge ready_slave);  assert(data_out_slave, data_in_master);
  @(posedge ready_master); data_in_master <= 8'hB1;
  @(posedge ready_slave);  assert(data_out_slave, data_in_master);
  @(posedge ready_master); data_in_master <= 8'hC1;
  @(posedge ready_slave);  assert(data_out_slave, data_in_master);
  @(posedge ready_master); data_in_master <= 8'hD1;
  @(posedge ready_slave);  assert(data_out_slave, data_in_master);

  // Читаем слово из SLAVE 32'hA2B2C2D2
  @(posedge start_transaction);
  @(posedge ready_slave);  data_in_slave <= 8'hA2;
  @(posedge ready_master); assert(data_out_master, data_in_slave);
  @(posedge ready_slave);  data_in_slave <= 8'hB2;
  @(posedge ready_master); assert(data_out_master, data_in_slave);
  @(posedge ready_slave);  data_in_slave <= 8'hC2;
  @(posedge ready_master); assert(data_out_master, data_in_slave);
  @(posedge ready_slave);  data_in_slave <= 8'hD2;
  @(posedge ready_master); assert(data_out_master, data_in_slave);

end

initial begin
  $dumpfile("i2c_tb.vcd");
  $dumpvars(0, i2c_tb);
end

endmodule