module mult_inner
    #( parameter DATA_WIDTH = 32 )
    ( input signed [DATA_WIDTH/2-1:0] dataa
    , input signed [DATA_WIDTH/2-1:0] datab
    , output signed [DATA_WIDTH-1:0] result
    );

assign result = dataa * datab;

endmodule
