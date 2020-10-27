`timescale 1 ms/ 1 ms

module nitta_to_i2c_splitter #
    ( parameter DATA_WIDTH     = 32
    , parameter ATTR_WIDTH     = 4
    , parameter I2C_DATA_WIDTH = 8
    )
    ( input                       clk
    , input                       rst

    , input                       i2c_ready
    , output [I2C_DATA_WIDTH-1:0] to_i2c

    , output                      splitter_ready
    , input  [DATA_WIDTH-1:0]     from_nitta
    );

localparam SUBFRAME_NUMBER = DATA_WIDTH / I2C_DATA_WIDTH;
localparam SUBFRAME_COUNTER_WIDTH = $clog2( SUBFRAME_NUMBER );

reg [DATA_WIDTH-1:0] data;
reg [SUBFRAME_COUNTER_WIDTH-1:0] counter;

wire [SUBFRAME_COUNTER_WIDTH-1:0] counter_wire = i2c_ready && wait_i2c_ready ? counter + 1 : counter;
wire [$clog2( DATA_WIDTH )-1:0] shift = (SUBFRAME_NUMBER - counter_wire - 1) * I2C_DATA_WIDTH;
assign to_i2c = subframe[ I2C_DATA_WIDTH-1 : 0 ];
reg [I2C_DATA_WIDTH-1:0] subframe;

always @( posedge clk )
    if ( rst ) subframe <= 0;
    else subframe <= from_nitta >> shift;

reg wait_i2c_ready;

always @( posedge clk ) begin
    if ( rst ) begin
        data <= 0;
        counter <= 0;
        if ( i2c_ready ) wait_i2c_ready <= 0;
        else wait_i2c_ready <= 1;
    end else if ( wait_i2c_ready && i2c_ready ) begin
        if ( counter == SUBFRAME_NUMBER - 1 ) begin
            data <= from_nitta;
            counter <= 0;
        end else begin
            counter <= counter + 1;
        end
        wait_i2c_ready <= 0;
    end else if ( !wait_i2c_ready && !i2c_ready ) begin
        wait_i2c_ready <= 1;
    end
end

assign splitter_ready = counter == SUBFRAME_NUMBER - 1
                     && wait_i2c_ready
                     && i2c_ready ;

endmodule
