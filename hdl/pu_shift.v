// `timescale 1ns / 1ps

/*

Длинна шага в step-ах зависит от того, сколько времени считается функция. Надо сделать два сдвига -
делаем 2 такта. Надо 4 - 4 такта, надо 9, делаем один шаг на 8 бит и 1 на 1.

*/

module pu_shift
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   )
  ( input  wire                  clk
  , input  wire                  signal_work
  , input  wire                  signal_direction // 1 - left, 0 - right
  , input  wire                  signal_mode      // 1 - arithmetic; 0 - logic
  , input  wire                  signal_step      // 1 - 8 bit, 0 - 1 bit
  , input  wire                  signal_init
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out
  );

wire n = signal_step ? 8 : 1;

reg signed [DATA_WIDTH-1:0] data;
reg [ATTR_WIDTH-1:0] attr;
always @(posedge clk) begin
    if ( signal_init ) { attr, data } <= { attr_in, data_in };
    else if ( signal_work ) begin
      if ( signal_direction ) data <= signal_mode ? data <<< n : data << n;
      else                    data <= signal_mode ? data >>> n : data >> n;
    end
  end

always @(posedge clk) { attr_out, data_out } <= signal_oe ? { attr, data } : 0;

endmodule
