module pu_multiplexer
    #( parameter DATA_WIDTH = 32
    , parameter SEL_BITS    = 2
    )
    ( input  wire                  clk
    , input  wire                  rst
    
    , input  wire                  signal_wr
    , input  wire [DATA_WIDTH-1:0] data_in
    , input  wire                  signal_oe

    , output reg  [DATA_WIDTH-1:0] data_out
    , output reg  [SEL_BITS-1:0]   sel_out
    );

reg [DATA_WIDTH-1:0] data_buffer [0:(2**SEL_BITS)-1];
reg [SEL_BITS-1:0]   current_sel;
reg                   wr_phase;


always @(posedge clk) begin
    if (rst) begin
        current_sel <= 0;
        wr_phase <= 0;
        for (integer i = 0; i < 2**SEL_BITS; i = i + 1)
            data_buffer[i] <= 0;
    end else begin
        if (signal_wr) begin
            if (wr_phase == 0) begin
                current_sel <= data_in[SEL_BITS-1:0];
                wr_phase <= 1;
            end else begin
                data_buffer[current_sel] <= data_in;
                wr_phase <= 0;
            end
        end
    end
end

always @(posedge clk) begin
    if (signal_oe) begin
        data_out <= data_buffer[current_sel];
        sel_out <= current_sel;
    end else begin
        data_out <= {DATA_WIDTH{1'bz}};
        sel_out <= {SEL_BITS{1'bz}};
    end
end

endmodule
