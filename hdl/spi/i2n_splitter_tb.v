`timescale 1 ms/ 1 ms
module spi_to_nitta_splitter_tb();

reg  clk;
reg  rst;

reg spi_ready;
reg [7:0] from_spi;

wire splitter_ready;
wire [31:0] to_nitta;

i2n_splitter
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 0 )
   , .SPI_DATA_WIDTH( 8 )
   ) i2n_splitter
  ( .clk( clk )
  , .rst( rst )

  , .spi_ready( spi_ready )
  , .from_spi( from_spi )

  , .splitter_ready( splitter_ready )
  , .to_nitta( to_nitta )
  );

initial i2n_splitter.data = 0;

always begin
  clk <= 1;
  rst <= 1;
  repeat(4) #5 clk <= ~clk;
  rst <= 0;
  forever #5 clk <= ~clk;
end 

task assert32;
  input [31:0] a;
  input [31:0] b;
  begin
    if ( !(a === b) ) begin
      $display("ASSERT FAIL: %h === %h", a, b);
      $finish(2);
    end
  end
endtask

initial begin

  @(negedge rst);

  // Read word 32 bits 32'hA3A2A1A0
  spi_ready <= 1;       repeat(10)  @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hA0; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hA1; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hA2; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hA3; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);  
  spi_ready <= 1; rst <= 1;         @(posedge clk);
  assert32(to_nitta, 32'hA3A2A1A0);
  spi_ready <= 1; rst <= 0;         @(posedge clk);

  // Read word 32 bits 32'hB3B2B1B0
  spi_ready <= 1;       repeat(10)  @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hB0; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hB1; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hB2; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);
  spi_ready <= 1; from_spi = 8'hB3; @(posedge clk);
  spi_ready <= 0;                   @(posedge clk);  
  spi_ready <= 1; rst <= 1;         @(posedge clk);
  assert32(to_nitta, 32'hB3B2B1B0);
  spi_ready <= 1; rst <= 0;         @(posedge clk);

  repeat(10) @(posedge clk); $finish;
end

initial begin
  $dumpfile("i2n_splitter_tb.vcd");
  $dumpvars(-1, i2n_splitter_tb);
end 

endmodule
