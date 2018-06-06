module pu_div 
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter INVALID    = 0 
   , parameter PIPE       = 4
   )

  ( input  wire                  clk
  , input  wire                  rst

  , input  wire                  signal_wr  
  , input  wire                  signal_wr_sel
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH-1:0] attr_in

  , input  wire                  signal_oe
  , input  wire                  signal_oe_sel
  , output wire [DATA_WIDTH-1:0] data_out 
  , output wire [ATTR_WIDTH-1:0] attr_out
  );

                                              
reg [DATA_WIDTH-1:0]             arg [0:1];
reg [DATA_WIDTH-1:0]             arg_latch;
reg                              attr_latch;
reg                              attr [0:1];

always @(posedge clk) begin
  if (rst) begin
    arg[0] <= 0;
    arg[1] <= 0;
    arg_latch <= 0;
    attr[0] <= 0;
    attr[1] <= 0;
  end else begin
    if (signal_wr && !signal_wr_sel) begin
      arg_latch <= data_in[DATA_WIDTH-1:0];
      attr_latch <= attr_in;   
    end else if (signal_wr && signal_wr_sel) begin
      arg[0] <= arg_latch;
      attr[0] <= attr_latch;
      arg[1] <= data_in;
      attr[1] <= attr_in;
    end
  end
end

reg [PIPE-1:0] attr_wait;
reg            comm_attr;

always @(posedge clk) begin
  if ( rst ) begin
    attr_wait <= 0;
    comm_attr <= 0;
  end else begin
    attr_wait[0] <= attr[0] || attr[1] || arg[1] == 0;
    attr_wait[PIPE-1 : 1] <= attr_wait[ PIPE-2 : 0];
  end
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
  defparam
    div_i1.LPM_DIVIDE_component.lpm_pipeline = 8;


reg                  invalid_result;
reg [DATA_WIDTH-1:0] data_divresult;

always @(posedge clk) begin
  if ( rst ) begin
    invalid_result <= 0;
    data_divresult <= 0;
  end else begin
    if ( signal_oe) begin
      invalid_result <= attr_wait[PIPE-1];
      if ( signal_oe_sel ) begin
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