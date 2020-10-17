`timescale 1 ms/ 1 ms

module i2n_splitter
    #( parameter DATA_WIDTH     = 32
    , parameter ATTR_WIDTH     = 4
    , parameter SPI_DATA_WIDTH = 8
    )
    ( input                           clk
    , input                           rst

    , input                           spi_ready
    , input      [SPI_DATA_WIDTH-1:0] from_spi

    , output                          splitter_ready
    , output     [DATA_WIDTH-1:0]     to_nitta
    );

localparam SUBFRAME_NUMBER = DATA_WIDTH / SPI_DATA_WIDTH;
localparam SUBFRAME_COUNTER_WIDTH = $clog2( SUBFRAME_NUMBER );

reg [DATA_WIDTH-1:0] data;
always @( posedge clk )
    if(spi_ready) data = {data[DATA_WIDTH - SPI_DATA_WIDTH - 1:0], from_spi};

reg [SUBFRAME_COUNTER_WIDTH:0] counter;
reg                            wait_spi_ready;
always @( posedge clk ) begin
    if ( rst | (counter == SUBFRAME_NUMBER & !wait_spi_ready) ) begin
        counter <= 0;
        wait_spi_ready <= 0;
    end else if(spi_ready && wait_spi_ready) begin
        if ( counter[SUBFRAME_COUNTER_WIDTH] ) begin
            counter <= 1;
        end else begin
            counter <= counter + 1;
        end
        wait_spi_ready <= 0;
    end else if ( !wait_spi_ready && !spi_ready ) begin
        wait_spi_ready <= 1;
    end
end

assign splitter_ready = counter == SUBFRAME_NUMBER & !wait_spi_ready;
assign to_nitta = data;

endmodule
