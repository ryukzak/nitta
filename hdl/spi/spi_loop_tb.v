module spi_loop_tb();

reg clk;
reg rst;
reg [7:0] data_in;
wire [7:0] data_out;

                                                                                                       
initial                                                                                                   
  begin                                                                                                   
    $dumpfile("spi_loop_tb.vcd");                                                                 
    $dumpvars(0, spi_loop_tb);                                                                      
  end                                                                                     


wire miso;
wire mosi;
wire sclk;
wire cs;


// spi_loop dut(clk, rst, 8'he2 // data_in
spi_loop dut(clk, rst, data_in
, data_out, 1'b1,
  miso, mosi, sclk, cs, miso, mosi, sclk, cs
  );

initial begin                 
  data_in = 0;                                                                            
  clk = 1'b0;                                                                                             
  rst = 1'b1;                                                                                             
  repeat(2) #10 clk = ~clk;                                                                               
  rst = 1'b0;                                                                                             
  forever #10 clk = ~clk;                                                                                 
end           

always begin
  repeat (38) 
  @(posedge clk); 
  data_in <= data_in + 1;
end

initial
  begin
    repeat (2000) @(posedge clk);
    $finish;
  end

endmodule
