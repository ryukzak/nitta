module pu_div 
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter INVALID    = 0 
   , parameter CLK_LATENCY  = 32
   )
  ( input  wire                  clk
  , input  wire                  rst

  , input  wire                  signal_wr  
  , input  wire                  signal_sel
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH:0]   attr_in

  , input  wire                    res_select
  , input  wire                  signal_oe
  , output wire [DATA_WIDTH-1:0] data_out 
  , output wire [ATTR_WIDTH:0]   attr_out
  );

                                              
reg [DATA_WIDTH-1:0]              arg [0:1];
reg                               arg_invalid [0:1];

always @(posedge clk) begin 
    
      arg[signal_sel] <= data_in[DATA_WIDTH-1:0];
      arg_invalid[signal_sel] <= attr_in[INVALID]; 
    
end

wire [DATA_WIDTH-1:0]         quotient_result;
wire [DATA_WIDTH-1:0]         remain_result;
div div_i1
  ( .numer( arg[0] )
  , .denom( arg[1] )
  , .quotient( quotient_result )
  , .remain( remain_result )
  , .clock( clk )
  );


reg [CLK_LATENCY-1:0]               count;

always @(posedge clk) begin
  if (signal_wr) begin
      count <= CLK_LATENCY;
  end 
  else
    if (count > 1) begin
      count <= count - 1;
  end
end

reg                  invalid_result;
reg [DATA_WIDTH-1:0] data_divresult;

always @(posedge clk) begin
  if ( rst ) begin
    invalid_result <= 0;
  end else begin
    invalid_result <= arg[1] == 0;
    if ( count == 1 ) begin
      if ( res_select ) begin
        data_divresult <= quotient_result;
      end
      else
        data_divresult <= remain_result;
    end
  end
end

assign data_out = signal_oe ? data_divresult : 0;
assign attr_out = signal_oe ? ({ {(ATTR_WIDTH-1){1'b0}}, invalid_result } << INVALID) 
                              | {(ATTR_WIDTH-1){1'b0}} 
                            : 0;

endmodule