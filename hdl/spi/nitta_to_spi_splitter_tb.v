`timescale 1 ms/ 1 ms
module nitta_to_spi_splitter_tb();

reg  clk;
reg  rst;

reg spi_ready;
wire [7:0] to_spi;

wire splitter_ready;
reg [31:0] from_nitta;

nitta_to_spi_splitter 
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 0 )
   , .SPI_DATA_WIDTH( 8 )
   ) nitta_to_spi_splitter 
  ( .clk( clk )
  , .rst( rst )

  , .spi_ready( spi_ready )
  , .to_spi( to_spi )

  , .splitter_ready( splitter_ready )
  , .from_nitta( from_nitta )
  );

always begin
  clk <= 1;
  rst <= 1;
  repeat(4) #5 clk <= ~clk;
  rst <= 0;
  forever #5 clk <= ~clk;
end 

task assert8;
  input [7:0] a;
  input [7:0] b;
  begin
    if ( !(a === b) ) begin
      $display("ASSERT FAIL: %h === %h", a, b);
      $finish(2);
    end
  end
endtask

task assert1;
  input a;
  input b;
  begin
    if ( !(a === b) ) begin
      $display("ASSERT FAIL: %h === %h", a, b);
      $finish(2);
    end
  end
endtask

initial begin
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;

  @(negedge rst);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hA0); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hB1); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hB1); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hC2); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hC2); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hC2); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hC2); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hD3); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hD3); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hA0); assert1(splitter_ready, 1); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hA0); assert1(splitter_ready, 0); @(posedge clk);
  repeat(4) @(posedge clk);

  rst <= 1; @(posedge clk);
  rst <= 0; @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hA0); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hB1); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hB1); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hB1); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hB1); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hB1); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hC2); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hC2); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hC2); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hD3); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hD3); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hD3); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hA0); assert1(splitter_ready, 1); @(posedge clk);
  spi_ready <= 1; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hA0); assert1(splitter_ready, 0); @(posedge clk);
  spi_ready <= 0; from_nitta <= 32'hA0B1C2D3;   #1assert8(to_spi, 8'hA0); assert1(splitter_ready, 0); @(posedge clk);
  repeat(10) @(posedge clk); $finish;
end

initial begin
  $dumpfile("nitta_to_spi_splitter_tb.vcd");
  $dumpvars(-1, nitta_to_spi_splitter_tb);
end 

endmodule
