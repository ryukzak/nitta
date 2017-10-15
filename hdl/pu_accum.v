module pu_accum (
    clk,
    
    signal_init,
    signal_load,
    signal_neg,

    data_in,
    attr_in,

    signal_oe,
    data_out,
    attr_out
);

parameter DATA_WIDTH     = 32;
parameter ATTR_WIDTH     = 4; 

parameter SIGN     = 0;
parameter OVERFLOW = 1;


input clk;
input signal_load, signal_init, signal_neg;
input [DATA_WIDTH-1:0] data_in;
input [ATTR_WIDTH-1:0] attr_in;

input signal_oe;
output [DATA_WIDTH-1:0] data_out;
output [ATTR_WIDTH-1:0] attr_out;


reg [DATA_WIDTH-1:0]   ext_arg;
reg [DATA_WIDTH-1:0]   int_arg;
reg [DATA_WIDTH:0]  acc; // +1 на переполнение
reg overflow;

reg [DATA_WIDTH-1:0] data_out;
reg [ATTR_WIDTH-1:0] attr_out;


always @(posedge clk)
  if ( signal_load ) begin
    if ( signal_init ) begin
      overflow = attr_in[ OVERFLOW ];
      int_arg = 0;
    end else begin 
      overflow = overflow || attr_in[ OVERFLOW ];
      int_arg = acc[DATA_WIDTH:0];
    end
    ext_arg = signal_neg ? -data_in : data_in;
  end


wire [DATA_WIDTH:0]  wacc; // +1 на переполнение
wire carry;

// https://en.wikipedia.org/wiki/Two%27s_complement#Addition
assign { carry, wacc[DATA_WIDTH-2:0] } = ext_arg[DATA_WIDTH-2:0] + int_arg[DATA_WIDTH-2:0];
assign wacc[DATA_WIDTH:DATA_WIDTH-1]   = ext_arg[DATA_WIDTH-1] + int_arg[DATA_WIDTH-1] + carry;

always @(posedge clk) begin
    acc[DATA_WIDTH:0] <= wacc;
    overflow <= carry ^ acc[DATA_WIDTH];
  end


always @(posedge clk)
  if ( ~signal_oe )
    { attr_out, data_out } <= 0;
  else begin
    data_out           <= acc[DATA_WIDTH-1:0];
    attr_out[SIGN]     <= acc[DATA_WIDTH];
    attr_out[OVERFLOW] <= overflow;
  end             

endmodule
