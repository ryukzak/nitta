`timescale 1 ps / 1 ps

module mult_inner
   #( parameter DATA_WIDTH = 32
   )
  ( input [DATA_WIDTH/2-1:0] dataa
  , input [DATA_WIDTH/2-1:0] datab
  , output [DATA_WIDTH-1:0] result
  );

wire [DATA_WIDTH-1:0] sub_wire0;
assign result = sub_wire0[DATA_WIDTH-1:0];

lpm_mult lpm_mult_component (
    .dataa (dataa),
    .datab (datab),
    .result (sub_wire0),
    .aclr (1'b0),
    .clken (1'b1),
    .clock (1'b0),
    .sclr (1'b0),
    .sum (1'b0)
    );
defparam
    lpm_mult_component.lpm_hint = "MAXIMIZE_SPEED=5",
    lpm_mult_component.lpm_representation = "SIGNED",
    lpm_mult_component.lpm_type = "LPM_MULT",
    lpm_mult_component.lpm_widtha = DATA_WIDTH/2,
    lpm_mult_component.lpm_widthb = DATA_WIDTH/2,
    lpm_mult_component.lpm_widthp = DATA_WIDTH;

endmodule
