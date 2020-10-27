`timescale 1 ms/ 1 ms
module pu_slave_spi_driver_tb();

parameter DATA_WIDTH = 8;

reg  clk;
reg  rst;

reg [DATA_WIDTH-1:0] data_in;
wire [DATA_WIDTH-1:0] data_out;
wire ready, prepare;

reg mosi, sclk, cs;
wire miso;


pu_slave_spi_driver
  #( .DATA_WIDTH( DATA_WIDTH )
   ) spi_slave_driver
  ( .clk( clk )
  , .rst( rst )

  , .data_in( ready ? data_out : data_in )
  , .data_out( data_out )
  , .ready( ready )
  , .prepare( prepare )

  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );

always @(posedge clk) begin
    if ( rst ) begin
        data_in <= 0;
    end else begin
        if ( ready ) begin
            data_in <= data_out;
        end
    end
end

always begin
  clk <= 1;
  rst <= 1;
  repeat(6) #5 clk <= ~clk;
  rst <= 0;
  forever #5 clk <= ~clk;
end


initial begin
  mosi <= 0; sclk <= 0; cs <= 1; data_in <= 4'h0;
  @(negedge rst); @(posedge clk);





  ////////////////////////////////////////////////////////////////////////
  // one value in frame.
  ////////////////////////////////////////////////////////////////////////
  mosi <= 0; sclk <= 0; cs <= 0;   repeat(1) @(posedge clk);
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);
  end
  mosi <= 0; sclk <= 0; cs <= 1;   repeat(10) @(posedge clk);





  ////////////////////////////////////////////////////////////////////////
  // one value in frame.
  ////////////////////////////////////////////////////////////////////////
  mosi <= 0; sclk <= 0; cs <= 0;   repeat(2) @(posedge clk);
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);
  end
  mosi <= 0; sclk <= 0; cs <= 1;   repeat(10) @(posedge clk);





  ////////////////////////////////////////////////////////////////////////
  // two value in frame.
  ////////////////////////////////////////////////////////////////////////
  mosi <= 0; sclk <= 0; cs <= 0;   repeat(1) @(posedge clk);
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);
  end
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);
  end
  mosi <= 0; sclk <= 0; cs <= 1;   repeat(10) @(posedge clk);





  ////////////////////////////////////////////////////////////////////////
  // two value in frame.
  ////////////////////////////////////////////////////////////////////////
  mosi <= 0; sclk <= 0; cs <= 0;   repeat(2) @(posedge clk);
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);
  end
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(2) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(2) @(posedge clk);
  end
  mosi <= 0; sclk <= 0; cs <= 1;   repeat(10) @(posedge clk);




  ////////////////////////////////////////////////////////////////////////
  // four value in frame.
  ////////////////////////////////////////////////////////////////////////
  mosi <= 0; sclk <= 0; cs <= 0;   repeat(1) @(posedge clk);
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);
  end
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);
  end
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);
  end
  begin
    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);


    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 0; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 0; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);

    mosi <= 1; sclk <= 1; cs <= 0; repeat(1) @(posedge clk);
    mosi <= 1; sclk <= 0; cs <= 0; repeat(1) @(posedge clk);
  end
  mosi <= 0; sclk <= 0; cs <= 1;   repeat(10) @(posedge clk);

  $finish;
end

initial begin
  $dumpfile("pu_slave_spi_driver_tb.vcd");
  $dumpvars(-1, pu_slave_spi_driver_tb);
end

endmodule
