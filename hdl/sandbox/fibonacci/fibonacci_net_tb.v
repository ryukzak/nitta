`timescale 1 ps / 1 ps
module fibonacci_net_tb();

reg clk, rst;
wire mosi, sclk, cs, miso;

fibonacci_net net
  ( .clk( clk )
  , .rst( rst )
  , .mosi( mosi ), .sclk( sclk ), .cs( cs ), .miso( miso )
  );

reg spi_start_transaction;
reg  [32-1:0] spi_master_in;
wire [32-1:0] spi_master_out;
wire spi_ready;
spi_master_driver
  #( .DATA_WIDTH( 32 )
   , .SCLK_HALFPERIOD( 1 )
   ) spi_master
  ( .clk( clk )
  , .rst( rst )
  , .start_transaction( spi_start_transaction )
  , .data_in( spi_master_in )
  , .data_out( spi_master_out )
  , .ready( spi_ready )
  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );
initial spi_master.inner.shiftreg <= 0;

initial begin
  spi_start_transaction <= 0; spi_master_in <= 0;
  @(negedge rst);
  repeat(8) @(posedge clk);

  spi_master_in = 32'h01234567;                        @(posedge clk);
  spi_start_transaction = 1;                           @(posedge clk);
  spi_start_transaction = 0;                           @(posedge clk);
  repeat(70) @(posedge clk);

  spi_master_in = 32'h89ABCDEF;                        @(posedge clk);
  spi_start_transaction = 1;                           @(posedge clk);
  spi_start_transaction = 0;                           @(posedge clk);
  repeat(70) @(posedge clk);

  spi_master_in = 32'h01234567;                        @(posedge clk);
  spi_start_transaction = 1;                           @(posedge clk);
  spi_start_transaction = 0;                           @(posedge clk);
  repeat(70) @(posedge clk);

  spi_master_in = 32'h89ABCDEF;                        @(posedge clk);
  spi_start_transaction = 1;                           @(posedge clk);
  spi_start_transaction = 0;                           @(posedge clk);
  repeat(70) @(posedge clk);
end


initial
  begin
    $dumpfile("fibonacci_net_tb.vcd");
    $dumpvars(0, fibonacci_net_tb);
  end


initial begin
  clk = 1'b0;
  rst = 1'b1;
  repeat(4) #10 clk = ~clk;
  rst = 1'b0;
  forever #10 clk = ~clk;
end


  initial
    begin
      // microcode when rst == 1 -> program[0], and must be nop for all PUs
      @(negedge rst); // Turn nitta processor on.
      // Start computational cycle from program[1] to program[n] and repeat.
      // Signals effect to processor state after first clk posedge.
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 0, "a1");if ( !( net.data_bus === 0) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 0, "a1");if ( !( net.data_bus === 0) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 1, "b1");if ( !( net.data_bus === 1) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 1, "c");if ( !( net.data_bus === 1) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 1, "a1");if ( !( net.data_bus === 1) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 1, "a1");if ( !( net.data_bus === 1) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 1, "b1");if ( !( net.data_bus === 1) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 2, "c");if ( !( net.data_bus === 2) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 1, "a1");if ( !( net.data_bus === 1) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 1, "a1");if ( !( net.data_bus === 1) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 2, "b1");if ( !( net.data_bus === 2) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 3, "c");if ( !( net.data_bus === 3) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 2, "a1");if ( !( net.data_bus === 2) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 2, "a1");if ( !( net.data_bus === 2) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 3, "b1");if ( !( net.data_bus === 3) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 5, "c");if ( !( net.data_bus === 5) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 3, "a1");if ( !( net.data_bus === 3) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 3, "a1");if ( !( net.data_bus === 3) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 5, "b1");if ( !( net.data_bus === 5) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 8, "c");if ( !( net.data_bus === 8) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 5, "a1");if ( !( net.data_bus === 5) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 5, "a1");if ( !( net.data_bus === 5) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 8, "b1");if ( !( net.data_bus === 8) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 13, "c");if ( !( net.data_bus === 13) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 8, "a1");if ( !( net.data_bus === 8) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 8, "a1");if ( !( net.data_bus === 8) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 13, "b1");if ( !( net.data_bus === 13) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 21, "c");if ( !( net.data_bus === 21) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 13, "a1");if ( !( net.data_bus === 13) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 13, "a1");if ( !( net.data_bus === 13) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 21, "b1");if ( !( net.data_bus === 21) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 34, "c");if ( !( net.data_bus === 34) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 21, "a1");if ( !( net.data_bus === 21) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 21, "a1");if ( !( net.data_bus === 21) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 34, "b1");if ( !( net.data_bus === 34) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 55, "c");if ( !( net.data_bus === 55) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 34, "a1");if ( !( net.data_bus === 34) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 34, "a1");if ( !( net.data_bus === 34) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 55, "b1");if ( !( net.data_bus === 55) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 89, "c");if ( !( net.data_bus === 89) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 55, "a1");if ( !( net.data_bus === 55) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 55, "a1");if ( !( net.data_bus === 55) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 89, "b1");if ( !( net.data_bus === 89) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 144, "c");if ( !( net.data_bus === 144) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 89, "a1");if ( !( net.data_bus === 89) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 89, "a1");if ( !( net.data_bus === 89) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 144, "b1");if ( !( net.data_bus === 144) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 233, "c");if ( !( net.data_bus === 233) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 144, "a1");if ( !( net.data_bus === 144) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 144, "a1");if ( !( net.data_bus === 144) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 233, "b1");if ( !( net.data_bus === 233) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 377, "c");if ( !( net.data_bus === 377) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 233, "a1");if ( !( net.data_bus === 233) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 233, "a1");if ( !( net.data_bus === 233) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 377, "b1");if ( !( net.data_bus === 377) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 610, "c");if ( !( net.data_bus === 610) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "0", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "1", net.data_bus); $write(" == %h (%s)", 377, "a1");if ( !( net.data_bus === 377) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "2", net.data_bus); $write(" == %h (%s)", 377, "a1");if ( !( net.data_bus === 377) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "3", net.data_bus); $write(" == %h (%s)", 610, "b1");if ( !( net.data_bus === 610) ) $display(" FAIL"); else $display();
      @(posedge clk); $write("%s, bus: %h", "4", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "5", net.data_bus); $display();
      @(posedge clk); $write("%s, bus: %h", "6", net.data_bus); $write(" == %h (%s)", 987, "c");if ( !( net.data_bus === 987) ) $display(" FAIL"); else $display();

      $finish;
    end

endmodule
