module bounce_filter #
        ( parameter DIV = 5
        )
    ( input rst
    , input clk
    , input in
    , output out
    );


generate
    if ( DIV == 0 || DIV == 1 ) begin
        assign out = in;

    end else begin
        reg [DIV-1:0] cnt;
        reg           acc;
        
        always @(posedge clk) begin
            if (rst) begin
                cnt <= 0;
                acc <= 0;
            end else if (in) begin
                if (cnt != {DIV{1'b1}}) cnt <= cnt + 1;
            end else begin
                if (cnt != {DIV{1'b0}}) cnt <= cnt - 1;
            end
            
            if (!(cnt[DIV-1] ^ cnt[DIV-2])) begin
                acc <= cnt[DIV-1];
            end

        end

        assign out = acc;

    end
endgenerate
    
endmodule
