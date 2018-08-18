module bounce_filter #
        ( parameter DIV = 20
        )
    ( input rst
    , input clk
    , input in
    , output out
    );

localparam CNT_WIDTH = $clog2( DIV );

reg [CNT_WIDTH-1:0] cnt;

generate
    if ( DIV == 0 ) begin
        assign out = in;
    end else begin
        reg buffer;
        assign out = buffer;
        always @(posedge clk) begin
            if ( rst ) begin
                cnt <= 0;
            end else if ( cnt == DIV ) begin
                cnt <= 0;
                buffer <= in;
            end else begin
                cnt <= cnt + 1;
            end
        end
    end
endgenerate
	
endmodule