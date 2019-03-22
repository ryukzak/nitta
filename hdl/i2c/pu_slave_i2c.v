`timescale 1 ms/ 1 ms

// FIXME: Почистить реализацию модуля. Перенести модули тестового окружения в поддиректорию "test".

// FIXME: Необходимо сделать получение данных через данный вычислительный блок.

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

    // nitta interface
    , input                     signal_wr
    , input    [DATA_WIDTH-1:0] data_in
    , input    [ATTR_WIDTH-1:0] attr_in

    , input                     signal_oe
    , output   [DATA_WIDTH-1:0] data_out
    , output   [ATTR_WIDTH-1:0] attr_out

    , output reg                flag_stop

    // I2C interface
    , input scl
    , inout sda

    , output D4
    , output D5
    , output D6
    , output D7
    );


///////////////////////////////////////////////////////////
// [NITTA >>> SPI]

// send_buffers
reg send_buffer_sel;

wire send_buffer_wr[1:0];
wire send_buffer_oe[1:0];
wire [DATA_WIDTH-1:0] send_buffer_data_in[1:0];
wire [DATA_WIDTH-1:0] send_buffer_data_out[1:0];

generate
    genvar i;
    for ( i = 0; i < 2; i = i + 1 ) begin : send_buffer_i
        buffer #
                ( .BUF_SIZE( BUF_SIZE )
                , .DATA_WIDTH( DATA_WIDTH )
                ) send_buffer // from nitta to spi
            ( .clk( clk )
            , .rst( rst || flag_stop )

            , .wr( send_buffer_wr[i] )
            , .data_in( send_buffer_data_in[i] )

            , .oe( send_buffer_oe[i] )
            , .data_out( send_buffer_data_out[i] )
            ); 
    end
endgenerate

// signal_wr can be received only from the nitta's side.
assign send_buffer_wr[0] =  send_buffer_sel ? signal_wr : 1'h0;
assign send_buffer_wr[1] = !send_buffer_sel ? signal_wr : 1'h0;
assign send_buffer_data_in[0] =  send_buffer_sel ? data_in : 0;
assign send_buffer_data_in[1] = !send_buffer_sel ? data_in : 0;

// signal_oe can be received only from the spi driver's side (splitter).
assign send_buffer_oe[0] = !send_buffer_sel ? splitter_ready : 1'h0;
assign send_buffer_oe[1] =  send_buffer_sel ? splitter_ready : 1'h0;
wire [DATA_WIDTH-1:0] nitta_to_splitter =  send_buffer_data_out[send_buffer_sel];

// splitter: translate from DATA_WIDTH to SPI_DATA_WIDTH
wire splitter_ready;
wire [SPI_DATA_WIDTH-1:0] splitter_to_spi;
nitta_to_spi_splitter #
        ( .DATA_WIDTH( DATA_WIDTH )
        , .ATTR_WIDTH( ATTR_WIDTH )
        , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
        ) nitta_to_spi_splitter 
    ( .clk( clk )
    , .rst( rst || flag_stop )

    , .spi_ready( i2c_prepare )
    , .to_spi( splitter_to_spi )

    , .splitter_ready( splitter_ready )
    , .from_nitta( nitta_to_splitter )
    );

///////////////////////////////////////////////////////////
// [SPI >>> NITTA]

// splitter: translate from SPI_DATA_WIDTH to DATA_WIDTH
wire spi_ready;
wire [SPI_DATA_WIDTH-1:0] splitter_from_spi;
wire splitter_ready_sn;
wire [DATA_WIDTH-1:0] to_nitta;
spi_to_nitta_splitter #
        ( .DATA_WIDTH( DATA_WIDTH )
        , .ATTR_WIDTH( ATTR_WIDTH )
        , .SPI_DATA_WIDTH( SPI_DATA_WIDTH )
        ) spi_to_nitta_splitter 
    ( .clk( clk )
    , .rst( rst || flag_stop )
    , .spi_ready( i2c_ready_write )
    , .from_spi( splitter_from_spi )
    , .splitter_ready( splitter_ready_sn )
    , .to_nitta( to_nitta )
    );

wire receive_buffer_wr[1:0];
wire receive_buffer_oe[1:0];
wire receive_buffer_fs[1:0];
wire [DATA_WIDTH-1:0] receive_buffer_data_in[1:0];
wire [DATA_WIDTH-1:0] receive_buffer_data_out[1:0];

generate
    genvar j;
    for ( j = 0; j < 2; j = j + 1 ) begin : receive_buffer_j
        buffer #
                ( .DATA_WIDTH( DATA_WIDTH )
                , .BUF_SIZE( BUF_SIZE )
                ) receive_buffer
            ( .clk( clk )
            , .rst( rst || receive_buffer_fs[j] )

            , .wr( receive_buffer_wr[j] )
            , .data_in( receive_buffer_data_in[j] )

            , .oe_other( receive_buffer_oe[j] )
            , .data_out_other( receive_buffer_data_out[j] )
            ); 
    end
endgenerate

assign receive_buffer_wr[0] =  send_buffer_sel ? splitter_ready_sn : 1'h0;
assign receive_buffer_wr[1] = !send_buffer_sel ? splitter_ready_sn : 1'h0;
assign receive_buffer_fs[0] = !send_buffer_sel ? flag_stop : 1'h0;
assign receive_buffer_fs[1] =  send_buffer_sel ? flag_stop : 1'h0;
assign receive_buffer_data_in[0] =  send_buffer_sel ? to_nitta : 0;
assign receive_buffer_data_in[1] = !send_buffer_sel ? to_nitta : 0;
assign receive_buffer_oe[0] = !send_buffer_sel ? signal_oe : 1'h0;
assign receive_buffer_oe[1] =  send_buffer_sel ? signal_oe : 1'h0;

wire f_scl;
wire i2c_prepare, i2c_ready_write;

pu_i2c_slave_driver #
  ( .I2C_DATA_WIDTH( 8 )
  , .DATA_WIDTH( 64 )
  , .ADDRES_DEVICE( 7'h47 )
  ) driver_slave
  ( .clk( clk )
  , .rst( rst )
  , .scl( f_scl )
  , .sda( sda )
  , .data_in( splitter_to_spi )
  , .ready_write( i2c_ready_write )
  , .i2c_prepare( i2c_prepare )
  );

// bounce filter
bounce_filter #( .DIV(BOUNCE_FILTER) ) f_scl_filter ( rst, clk, scl, f_scl );

///////////////////////////////////////////////////////////
// Control logic

reg curr_sda;
always @(posedge clk) begin
  if (rst) begin
    curr_sda   <= 1'b1;
  end else begin
    curr_sda <= sda;
  end
end

always @(posedge clk) begin
  if (rst) begin
    flag_stop <= 1'b0;
  end else begin
    if (scl) begin
      if(curr_sda != sda && sda == 1'b1) begin
        flag_stop <= 1'b1;
      end else begin
        flag_stop <= 1'b0;
      end 
    end   
  end
end

always @( posedge clk ) begin
    if ( rst ) send_buffer_sel <= 0;
    else if ( signal_cycle && scl && sda ) send_buffer_sel <= !send_buffer_sel;
end

reg [1:0] count_word;
always @( posedge clk ) begin
    if ( rst | flag_stop )        count_word <= 0;
    else if ( splitter_ready_sn ) count_word <= count_word + 1;
end

reg capture_word;
always @(posedge clk) begin
    if ( rst )            capture_word <= 0;
    else if ( flag_stop ) capture_word <= count_word != SIZE_WORDS;
end

assign attr_out[3:1] = 3'b000;
assign attr_out[INVALID] = capture_word;

assign data_out   = receive_buffer_oe[1] ? receive_buffer_data_out[1] : 
                    receive_buffer_oe[0] ? receive_buffer_data_out[0] : {DATA_WIDTH{1'b0}};

endmodule
