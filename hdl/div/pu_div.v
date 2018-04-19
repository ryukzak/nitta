module pu_div 
  #( parameter DATA_WIDTH = 32
   , parameter ATTR_WIDTH = 4
   , parameter INVALID    = 0 
   , parameter PIPE       = 4
   )

  ( input  wire                  clk
  , input  wire                  rst

  , input  wire                  signal_wr  
  , input  wire                  signal_sel
  , input  wire [DATA_WIDTH-1:0] data_in
  , input  wire [ATTR_WIDTH:0]   attr_in

  , input  wire                  res_select
  , input  wire                  signal_oe
  , output wire [DATA_WIDTH-1:0] data_out 
  , output wire [ATTR_WIDTH:0]   attr_out
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
    attr[0] <=0;
    attr[1] <=0;
  end else begin
    if (signal_wr && !signal_sel) begin
      arg_latch <= data_in[DATA_WIDTH-1:0];
      attr_latch <= attr_in;   
    end else if (signal_wr && signal_sel) begin
        arg[0] <= arg_latch;
        arg[1] <= data_in;
        attr[0] <=attr_latch;
        attr[1] <=attr_in;
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
    // if (signal_wr) begin
    //   if ( attr[0] || attr[1] ) begin
    //     comm_attr <= 1;
    //   end else if (arg[1] == 0) begin
    //       comm_attr <= 1;
    //     end else comm_attr <= 0;
    // end
    // if (attr[0] || attr[1] || arg[1] == 0) begin
      comm_attr <= 1;
    // end else comm_attr <= 0;
    if (attr[0] || attr[1] || arg[1] == 0) begin
    attr_wait[0] <= !signal_sel ? comm_attr : 0;
    end 
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

reg                  invalid_result;
reg [DATA_WIDTH-1:0] data_divresult;
// reg [DATA_WIDTH-1:0] div_result[1:0];
// reg                  attr_to_ex;

always @(posedge clk) begin
  // div_result[0] <= quotient_result;
  // div_result[1] <= remain_result;
  if ( rst ) begin
    // div_result[0] <= 0;
    // div_result[1] <= 0;
    invalid_result <= 0;
    data_divresult <= 0;
  end else begin
    // div_result[0] <= quotient_result;
    // div_result[1] <= remain_result;
    // attr_to_ex <= attr_wait[PIPE];
    if ( signal_oe) begin
      invalid_result <= attr_wait[PIPE-1];
      if ( res_select ) begin
        data_divresult <= quotient_result;
        // data_divresult <= div_result[0];
      end
      else
        data_divresult <= remain_result;
        // data_divresult <= div_result[1];
    end
  end
end

assign data_out = signal_oe ? data_divresult : 0;
assign attr_out = signal_oe ? ({ {(ATTR_WIDTH-1){1'b0}}, invalid_result } << INVALID) 
                              | {(ATTR_WIDTH-1){1'b0}} 
                            : 0;

endmodule