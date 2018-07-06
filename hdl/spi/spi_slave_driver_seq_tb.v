`timescale 1 ms/ 1 ms
module spi_slave_driver_tb();

parameter DATA_WIDTH = 8;

reg  clk;
reg  rst;

reg [DATA_WIDTH-1:0] data_in;
wire [DATA_WIDTH-1:0] data_out;
wire ready;

reg mosi, sclk, cs;
wire miso;


spi_slave_driver2
  #( .DATA_WIDTH( DATA_WIDTH )
   ) spi_slave_driver 
  ( .clk( clk )
  , .rst( rst )

  , .data_in( data_in ) 
  , .data_out( data_out )  
  , .ready( ready )

  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );


always begin
  clk <= 1;
  rst <= 1;
  repeat(6) #5 clk <= ~clk;
  rst <= 0;
  forever #5 clk <= ~clk;
end 


initial begin
  mosi <= 0; sclk <= 0; cs <= 1; data_in <= 4'h0;

  @(negedge rst);
  mosi <= 0; sclk <= 0; cs <= 1; data_in <= 4'h0; @(posedge clk);
  mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  begin
    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  end


  begin
    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);


    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  end


  begin
    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);


    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  end

  mosi <= 0; sclk <= 0; cs <= 1; data_in <= 4'h0; repeat(10) @(posedge clk); 




  mosi <= 0; sclk <= 0; cs <= 1; data_in <= 4'h0; @(posedge clk);
  mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  begin
    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);


    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  end


  begin
    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);


    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  end


  begin
    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);


    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; data_in <= 0; repeat(4) @(posedge clk);
  end

  mosi <= 0; sclk <= 0; cs <= 1; data_in <= 4'h0; repeat(10) @(posedge clk); 

  $finish;
end

initial begin
  $dumpfile("spi_slave_driver_tb.vcd");
  $dumpvars(-1, spi_slave_driver_tb);
end 

endmodule
