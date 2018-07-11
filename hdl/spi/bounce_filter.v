module bounce_filter #
        ( parameter DIV = 20
        )
    ( input rst
    , input clk
    , input in
    , output reg out
    );

localparam CNT_WIDTH = $clog2( DIV );

reg [CNT_WIDTH-1:0] cnt;

always @(posedge clk) begin
    if ( rst ) begin
        cnt <= 0;
    end else if ( cnt == DIV ) begin
        cnt <= 0;
        out <= in;
    end else begin
        cnt <= cnt + 1;
    end
end
	
endmodule