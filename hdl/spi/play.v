module play (
  input CLOCK_50,
  // input clk,
  input RST, // dip 1
  input MODE, // dip 2, 0 - ext, 1 - int
  input SPEED, // key 0 - speed up
  input SEL, // dip3; 0 - direct, 1 - reverse 
  output [7:0] LED,

  output ext_master_mosi,
  input  ext_master_miso,
  output ext_master_sclk,
  output ext_master_cs,
  
  input  ext_slave_mosi,
  output ext_slave_miso,
  input  ext_slave_sclk,
  input  ext_slave_cs
  );


wire clk;
pll pll(
  .inclk0(CLOCK_50),
  .c0(clk)
  );
  
  
reg [31:0] counter_data;
always @(posedge clk) begin
   if (RST) counter_data <= 0;
   else counter_data <= counter_data + 1;
end

wire [7:0] counter_out;
assign counter_out = SPEED ? counter_data[31:24] : counter_data[27:20];
// assign counter_out = counter_data[7:0];



wire master_mosi, master_miso, master_sclk, master_cs;
wire slave_mosi, slave_miso, slave_sclk, slave_cs;
wire mosi, miso, sclk, cs;



assign ext_master_mosi =     master_mosi; 
assign            mosi =     master_mosi; 
assign     master_miso = MODE ? miso : ext_master_miso;
assign ext_master_sclk =     master_sclk;
assign            sclk = master_sclk;
assign ext_master_cs   =     master_cs;
assign            cs   =     master_cs;

assign     slave_mosi = MODE ? mosi : ext_slave_mosi;
assign ext_slave_miso =     slave_miso;
assign           miso =     slave_miso;
assign     slave_sclk = MODE ? sclk : ext_slave_sclk;
assign     slave_cs   = MODE ? cs   : ext_slave_cs;

//assign ext_master_sclk = clk;

spi_loop spi (
  .clk(clk),
  .rst(RST),

  .master_spi_mosi(master_mosi),
  .master_spi_miso(master_miso),
  .master_spi_sclk(master_sclk),
  .master_spi_cs(master_cs),

  .slave_spi_mosi(slave_mosi),
  .slave_spi_sclk(slave_sclk),
  .slave_spi_cs(slave_cs),
  .slave_spi_miso(slave_miso),

  .data_in(counter_out),
  .data_out(LED),
  .select(SEL)
  );
  defparam spi.TIMER = 18;


//wire slave_ready;
//reg [7:0] slave_in;
//wire [7:0] slave_out;
//
//
//spi_slave_driver slave (
//  .clk(clk),
//  .rst(RST),
//  
//  .data_in(slave_in),
//  .ready(slave_ready),
//  .data_out(slave_out),
//  
//  .miso(ext_slave_miso),
//  .mosi(ext_slave_mosi),
//  .sclk(ext_slave_sclk),
//  .cs(ext_slave_cs)
//);
//
////assign ext_master_mosi = ext_slave_mosi;
////assign ext_slave_miso = ext_master_miso;
////assign ext_master_sclk = ext_slave_sclk;
////assign ext_master_cs = ext_slave_cs;
//reg[7:0] LEDREG;
//assign LED = LEDREG;
//
//always @(posedge clk) 
//  if ( slave_ready ) begin
//    slave_in <= { slave_out[0], slave_out[1], slave_out[2], slave_out[3], slave_out[4], slave_out[5], slave_out[6], slave_out[7] };
//    LEDREG <= slave_out;
//  end



  
  
endmodule
