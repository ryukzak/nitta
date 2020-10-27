// FIXME: A separate module is required to suppress bounce.
// It has a counter that it is incremented if on line 1, and decremented if 0.
// Counter with saturation.
// When the value reaches the threshold, the filtered output switches.

module bounce_filter
   #( parameter DIV = 5
    )
  ( input rst
  , input clk
  , input in
  , output out
  );

reg [DIV-1:0] cnt;

generate
    if ( DIV == 0 || DIV == 1 ) begin
        assign out = in;
    end else begin
        assign out = cnt[DIV-1] & cnt[DIV-2] ? 1 : 0;
        always @(posedge clk) begin
            if (rst) begin
                cnt <= 0;
            end else if (in) begin
                if (cnt != {DIV{1'b1}}) cnt <= cnt + 1;
            end else begin
                if (cnt != {DIV{1'b0}}) cnt <= cnt - 1;
            end
        end
    end
endgenerate

endmodule
