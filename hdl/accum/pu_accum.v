module pu_accum
  #( parameter DATA_WIDTH = 4
   , parameter ATTR_WIDTH = 4
   , parameter OVERFLOW   = 0
   , parameter FLOAT      = 1
   )
  ( input  wire                  clk
  , input  wire                  rst
  , input  wire                  signal_load
  , input  wire                  signal_resetAcc
  , input  wire                  signal_neg
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , output reg  [DATA_WIDTH-1:0] data_out
  , output reg  [ATTR_WIDTH-1:0] attr_out
  );


reg [DATA_WIDTH-1:0]   ext_arg;
reg [DATA_WIDTH-1:0]   int_arg;
reg [DATA_WIDTH:0]     acc; // +1 on overflow
wire [DATA_WIDTH:0]   wacc; // +1 on overflow
reg overflow;

// 10 11
always @(posedge clk)
  if ( rst ) begin
    int_arg <= 0;
    ext_arg <= 0;
  end else if ( signal_load ) begin
    int_arg <= signal_resetAcc ? 0 : wacc[DATA_WIDTH-1:0];
    if ( FLOAT ) begin
      ext_arg <= signal_neg ? { ~data_in[31], data_in[30:0] } : data_in;
    end else begin
      ext_arg <= signal_neg ? -data_in : data_in;
    end
  end

generate
  if ( FLOAT ) begin
    wire [DATA_WIDTH-1:0] sum_res;
    wire sum_attr;
    float_sum #(
      .DATA_WIDTH(DATA_WIDTH)
    ) accum (
      .a(int_arg),
      .b(ext_arg),
      .result(sum_res),
      .attr(sum_attr)
    );

    assign wacc = sum_res;

    always @(posedge clk) begin
      if ( rst || !signal_oe )
        { attr_out, data_out } <= 0;
      else begin
        data_out <= sum_res;
        attr_out <= {{ATTR_WIDTH-1{1'b0}},sum_attr};
      end
    end
  end else begin
    wire carry;

    // https://en.wikipedia.org/wiki/Two%27s_complement#Addition
    assign { carry, wacc[DATA_WIDTH-2:0] } = ext_arg[DATA_WIDTH-2:0] + int_arg[DATA_WIDTH-2:0];
    assign wacc[DATA_WIDTH:DATA_WIDTH-1]   = ext_arg[DATA_WIDTH-1] + int_arg[DATA_WIDTH-1] + carry;


    always @(posedge clk)
      if ( rst ) begin
        acc <= 0;
        overflow <= 0;
      end else begin
        acc[DATA_WIDTH:0] <= wacc;

        if ( signal_load ) begin
          if ( signal_resetAcc ) begin
            overflow <= attr_in[ OVERFLOW ];
          end else begin
            overflow <= overflow || attr_in[ OVERFLOW ];
          end
        end else begin
            overflow <= carry ^ wacc[DATA_WIDTH];
        end
      end

    always @(posedge clk)
      if ( rst || !signal_oe )
        { attr_out, data_out } <= 0;
      else begin
        data_out           <= acc[DATA_WIDTH-1:0];
        attr_out[OVERFLOW] <= overflow;
      end
  end
endgenerate

endmodule
