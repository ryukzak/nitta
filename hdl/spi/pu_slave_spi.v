`timescale 1 ms/ 1 ms

// FIXME: Почистить реализацию модуля. Перенести модули тестового окружения в поддиректорию "test".

// FIXME: Необходимо сделать корректную работу с атрибутами (архитектура, testbench-и, испытания в железе).

module pu_slave_spi #
        ( parameter DATA_WIDTH     = 32
        , parameter ATTR_WIDTH     = 4
        , parameter SPI_DATA_WIDTH = 8
        , parameter BUF_SIZE       = 6
        , parameter BOUNCE_FILTER  = 4
        , parameter INVALID        = 0
        , parameter SIZE_WORDS     = 2
        )
    ( input                     clk
    , input                     rst
    , input                     signal_cycle

    // NITTA interface
    , input                     signal_wr // Write data to send buffer (n2i). A
                                          // cell will change automatically on
                                          // each writes.
    , input    [DATA_WIDTH-1:0] data_in
    , input    [ATTR_WIDTH-1:0] attr_in

    , input                     signal_oe // Read data from the current cell of
                                          // receiving buffer (i2n). To change
                                          // cell signal_oe and signal_wr should
                                          // be sent at one time.
    , output   [DATA_WIDTH-1:0] data_out
    , output   [ATTR_WIDTH-1:0] attr_out

    , output reg                flag_stop

    // SPI interface
    , input                     cs
    , input                     sclk
    , input                     mosi
    , output                    miso
    );


reg disabled = 0;
reg buffer_sel; // buffer selector



///////////////////////////////////////////////////////////
// data flow from NITTA to SPI driver (n2i)

wire                  n2i_buf_mode[1:0];
wire                  n2i_action[1:0];
wire [DATA_WIDTH-1:0] n2i_buf_data_out[1:0];

generate
    genvar i;
    for ( i = 0; i < 2; i = i + 1 ) begin : n2i_buf_i
        buffer #
                ( .BUF_SIZE( BUF_SIZE )
                , .DATA_WIDTH( DATA_WIDTH )
                , .I(i)
                ) n2i_buf // from nitta to spi
            ( .clk( clk )
            , .rst( rst | flag_stop )

            , .receive_mode( n2i_buf_mode[i] )
            , .action( n2i_action[i] )

            , .data_in( data_in )
            , .data_out( n2i_buf_data_out[i] )
            ); 
    end
endgenerate

assign n2i_buf_mode[0] = signal_cycle ? !buffer_sel :  buffer_sel;
assign n2i_buf_mode[1] = signal_cycle ?  buffer_sel : !buffer_sel;

assign n2i_action[0] = n2i_buf_mode[0] ? (signal_wr & !signal_oe) : n2i_splitter_ready ;
assign n2i_action[1] = n2i_buf_mode[1] ? (signal_wr & !signal_oe) : n2i_splitter_ready ;

// splitter: translate from DATA_WIDTH to SPI_DATA_WIDTH
wire [SPI_DATA_WIDTH-1:0] n2i_splitter_data_out;
wire                      n2i_splitter_ready;
wire spi_prepare;

n2i_splitter #
        ( .DATA_WIDTH( DATA_WIDTH )
        , .ATTR_WIDTH( ATTR_WIDTH )
        , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
        ) n2i_splitter
    ( .clk( clk )
    , .rst( rst | flag_stop )

    , .spi_ready( spi_prepare )
    , .to_spi( n2i_splitter_data_out )

    , .splitter_ready( n2i_splitter_ready )
    , .from_nitta( n2i_buf_data_out[buffer_sel] )
    );



///////////////////////////////////////////////////////////
// data flow from SPI driver to NITTA (i2n)

// splitter: translate from SPI_DATA_WIDTH to DATA_WIDTH
wire [SPI_DATA_WIDTH-1:0] i2n_splitter_from_spi;
wire i2n_splitter_ready;
wire [DATA_WIDTH-1:0] i2n_splitter_data_out;
wire spi_ready;

i2n_splitter #
        ( .DATA_WIDTH( DATA_WIDTH )
        , .ATTR_WIDTH( ATTR_WIDTH )
        , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
        ) i2n_splitter
    ( .clk( clk )
    , .rst( rst | flag_stop )

    , .spi_ready( spi_ready )
    , .from_spi( i2n_splitter_from_spi )

    , .splitter_ready( i2n_splitter_ready )
    , .to_nitta( i2n_splitter_data_out )
    );


wire                  i2n_buf_mode[1:0];
wire                  i2n_action[1:0];
wire [DATA_WIDTH-1:0] i2n_buf_data_out[1:0];

generate
    genvar j;
    for ( j = 0; j < 2; j = j + 1 ) begin : i2n_buf_j
        buffer #
                ( .DATA_WIDTH( DATA_WIDTH )
                , .BUF_SIZE( BUF_SIZE )
                , .I(j)
                ) i2n_buf
            ( .clk( clk )
            , .rst( rst )

            , .receive_mode( i2n_buf_mode[j] )
            , .action( i2n_action[j] )

            , .data_in( i2n_splitter_data_out )
            , .data_out( i2n_buf_data_out[j] )
            );
    end
endgenerate

// Buffer select can't wait one tick to buffer_sel update after new
// computational cycle start. We should predict buffer_sel change.
assign i2n_buf_mode[0] = signal_cycle ? !buffer_sel : buffer_sel;
assign i2n_buf_mode[1] = signal_cycle ?  buffer_sel : !buffer_sel;

assign i2n_action[0] = i2n_buf_mode[0] ? i2n_splitter_ready : (signal_oe & signal_wr);
assign i2n_action[1] = i2n_buf_mode[1] ? i2n_splitter_ready : (signal_oe & signal_wr);



///////////////////////////////////////////////////////////
// SPI driver
wire f_mosi, f_cs, f_sclk;

pu_slave_spi_driver #
        ( .DATA_WIDTH( SPI_DATA_WIDTH )
        ) spi_driver
    ( .clk( clk )
    , .rst( rst )
    , .data_in( n2i_splitter_data_out )
    , .data_out( i2n_splitter_from_spi )
    , .ready( spi_ready )
    , .prepare( spi_prepare )
    , .mosi( f_mosi )
    , .miso( miso )
    , .sclk( f_sclk )
    , .cs( f_cs )
    );

// bounce filter

// FIXME: looks bad. Original signals are synchoronised, but filtered signals not.
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_mosi_filter ( rst, clk, mosi, f_mosi );
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_cs_filter   ( rst, clk, cs,   f_cs   );
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_sclk_filter ( rst, clk, sclk, f_sclk );



///////////////////////////////////////////////////////////
// Control logic

reg prev_f_cs;
always @( posedge clk ) prev_f_cs <= f_cs;

always @( posedge clk ) begin
    if      ( rst ) buffer_sel <= 0;
    else if ( signal_cycle && f_cs ) buffer_sel <= !buffer_sel;
end

wire transport_end = !prev_f_cs && f_cs;

always @( posedge clk ) begin
    if      ( rst ) flag_stop <= 0;
    else if ( disabled ) flag_stop <= 1;
    else if ( transport_end ) flag_stop <= 1;
    else flag_stop <= 0;
end

reg [1:0] count_word;
always @( posedge clk ) begin
    if      ( rst | flag_stop )    count_word <= 0;
    else if ( i2n_splitter_ready ) count_word <= count_word + 1;
end

reg capture_word;
always @(posedge clk) begin
    if      ( rst )       capture_word <= 0;
    else if ( flag_stop ) capture_word <= count_word != SIZE_WORDS; // TODO: Фиксированное значение 
end

// FIXME:
assign attr_out[3:1] = 3'b000;
assign attr_out[INVALID] = capture_word;

assign data_out = signal_oe ? i2n_buf_data_out[buffer_sel] : {DATA_WIDTH{1'b0}};

endmodule
