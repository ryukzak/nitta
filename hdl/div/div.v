`timescale 1 ps / 1 ps
module div
   #( parameter DATA_WIDTH = 32
     , parameter PIPELINE   = 4
   )
  ( input clock
  , input [DATA_WIDTH-1:0] denom
  , input [DATA_WIDTH-1:0] numer
  , output [DATA_WIDTH-1:0] quotient
  , output [DATA_WIDTH-1:0] remain
  );

wire [DATA_WIDTH-1:0] sub_wire0;
wire [DATA_WIDTH-1:0] sub_wire1;
assign quotient = sub_wire0[DATA_WIDTH-1:0];
assign remain = sub_wire1[DATA_WIDTH-1:0];

lpm_divide	LPM_DIVIDE_component (
            .clock (clock),
            .denom (denom),
            .numer (numer),
            .quotient (sub_wire0),
            .remain (sub_wire1),
            .aclr (1'b0),
            .clken (1'b1));
defparam
    LPM_DIVIDE_component.lpm_drepresentation = "SIGNED",
    LPM_DIVIDE_component.lpm_hint = "MAXIMIZE_SPEED=6,LPM_REMAINDERPOSITIVE=TRUE",
    LPM_DIVIDE_component.lpm_nrepresentation = "SIGNED",
    LPM_DIVIDE_component.lpm_pipeline = PIPELINE,
    LPM_DIVIDE_component.lpm_type = "LPM_DIVIDE",
    LPM_DIVIDE_component.lpm_widthd = DATA_WIDTH,
    LPM_DIVIDE_component.lpm_widthn = DATA_WIDTH;

endmodule
