`timescale 1 ms/ 1 ms

module n2i_splitter
    #( parameter DATA_WIDTH     = 32
    , parameter ATTR_WIDTH     = 4
    , parameter SPI_DATA_WIDTH = 8
    )
    ( input                       clk
    , input                       rst

    , input                       spi_ready
    , output [SPI_DATA_WIDTH-1:0] to_spi

    , output                      splitter_ready
    , input  [DATA_WIDTH-1:0]     from_nitta
    );

localparam SUBFRAME_NUMBER = DATA_WIDTH / SPI_DATA_WIDTH;
localparam SUBFRAME_COUNTER_WIDTH = $clog2( SUBFRAME_NUMBER );

reg [DATA_WIDTH-1:0] data;
reg [SPI_DATA_WIDTH-1:0] subframe;
reg [SUBFRAME_COUNTER_WIDTH-1:0] counter;
reg wait_spi_ready;

wire [SUBFRAME_COUNTER_WIDTH-1:0] counter_wire = spi_ready && wait_spi_ready ? counter + 1 : counter;
wire [$clog2( DATA_WIDTH )-1:0] shift = (SUBFRAME_NUMBER - counter_wire - 1) * SPI_DATA_WIDTH;
assign to_spi = subframe[ SPI_DATA_WIDTH-1 : 0 ];


always @( posedge clk )
    if ( rst ) subframe <= 0;
    else subframe <= from_nitta >> shift;


always @( posedge clk ) begin
    if ( rst ) begin
        data <= 0;
        counter <= 0;
        if ( spi_ready ) wait_spi_ready <= 0;
        else wait_spi_ready <= 1;
    end else if ( wait_spi_ready && spi_ready ) begin
        if ( counter == SUBFRAME_NUMBER - 1 ) begin
            data <= from_nitta;
            counter <= 0;
        end else begin
            counter <= counter + 1;
        end
        wait_spi_ready <= 0;
    end else if ( !wait_spi_ready && !spi_ready ) begin
        wait_spi_ready <= 1;
    end
end

assign splitter_ready = counter == SUBFRAME_NUMBER - 1
                     && wait_spi_ready
                     && spi_ready ;

endmodule
