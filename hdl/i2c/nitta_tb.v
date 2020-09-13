`timescale 1 ms/ 1 ms

module nitta_tb
  #( parameter I2C_DATA_WIDTH    = 8
   , parameter DATA_WIDTH        = 32
   , parameter ADDRES_DEVICE     = 7'h47
   , parameter ADDRES_BAD_DEVICE = 7'h44
   )
  ();

reg clk, rst, scl, f_sda;
wire sda;

reg [3:0] dips;

assign sda = f_sda;

localparam READ  = 1;
localparam WRITE = 0;

nitta #() driver_slave
  ( .external_clk( clk )

  , .scl( scl )
  , .sda( sda )

  , .dips( dips )
  );

always begin
  #5 clk <= ~clk;
end

task read_byte;
  begin
    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 1 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 2 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 3 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 4 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 5 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 6 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 7 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = 1'bz;                       // !-- Read 8 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact
  end
endtask

task write_ack;
  begin
    @(negedge clk);
    f_sda = 1'b0;                       // !-- Read ACK
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact
  end
endtask

task write_byte;
  input [7:0] data;
  begin
    f_sda = data[7];                       // !-- Read 1 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = data[6];                       // !-- Read 2 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = data[5];                       // !-- Read 3 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = data[4];                       // !-- Read 4 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = data[3];                       // !-- Read 5 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = data[2];                       // !-- Read 6 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = data[1];                       // !-- Read 7 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    @(negedge clk);
    f_sda = data[0];                       // !-- Read 8 bit
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact
  end
endtask

task read_ack;
  begin
    @(negedge clk);
    f_sda = 1'bz;                       // !-- Wait ACK
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact
  end
endtask

task stop_transfer;
  begin
    @(negedge clk);
    f_sda = 1'b0;                       // !-- Set zero

    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
    f_sda = 1'b1;                       // !-- Set Stop
  end
endtask

task start_transfer;
  begin
    repeat(5) @(posedge clk);
    f_sda = 0;
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Start transfer ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
    end
endtask

task read_addres;
  input [6:0] addres;
  input       rw;
  begin
    f_sda = addres[6];       // !-- Set the 1 bit  ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    f_sda = addres[5];       // !-- Set the 2 bit  ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    f_sda = addres[4];       // !-- Set the 3 bit  ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    f_sda = addres[3];       // !-- Set the 4 bit  ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    f_sda = addres[2];       // !-- Set the 5 bit  ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    f_sda = addres[1];       // !-- Set the 6 bit  ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    f_sda = addres[0];       // !-- Set the 7 bit  ADDRES_DEVICE
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact

    f_sda = rw;                       // !-- Set READ
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 1; // !-- Tact
    repeat(5) @(posedge clk);           // !-- delay
                               scl = 0; // !-- Tact
  end
endtask

task step_write;
  input [31:0] word_1;
  input [31:0] word_2;
  begin
    start_transfer();
  read_addres(ADDRES_DEVICE, WRITE);
  read_ack();

  write_byte(word_1[31:24]);
  read_ack();

  write_byte(word_1[23:16]);
  read_ack();

  write_byte(word_1[15:8]);
  read_ack();

  write_byte(word_1[7:0]);
  read_ack();

  write_byte(word_2[31:24]);
  read_ack();

  write_byte(word_2[23:16]);
  read_ack();

  write_byte(word_2[15:8]);
  read_ack();

  write_byte(word_2[7:0]);
  read_ack();

  stop_transfer();
    end
endtask

task step_read;
  begin
  start_transfer();
  read_addres(ADDRES_DEVICE, READ);
  read_ack();

  read_byte();
  write_ack();

  read_byte();
  write_ack();

  read_byte();
  write_ack();

  read_byte();
  write_ack();

  read_byte();
  write_ack();

  read_byte();
  write_ack();

  read_byte();
  write_ack();

  read_byte();
  write_ack();

  stop_transfer();
    end
endtask

initial begin
  clk   <= 0;
  f_sda <= 1;
  scl   <= 1;
  dips[3]   <= 0; @(posedge clk);
  dips[3]   <= 1; @(posedge clk);
  dips[3]   <= 0;
end

initial begin
  @(negedge dips[3]); repeat(10) @(posedge clk);


  step_write(32'h00000003, 32'h00000004);
  step_read();
  step_read();


  repeat(10) @(posedge clk); $finish;
end

initial begin
  $dumpfile("nitta_tb.vcd");
  $dumpvars(0, nitta_tb);
end

endmodule
