module fibonacci_net
  ( input                     clk
  , input                     rst
  , input mosi, input sclk, input cs
  , output miso
  );

parameter MICROCODE_WIDTH = 27;
parameter DATA_WIDTH = 32;
parameter ATTR_WIDTH = 4;

// Sub module instances
wire [MICROCODE_WIDTH-1:0] control_bus;
wire [DATA_WIDTH-1:0] data_bus;
wire [ATTR_WIDTH-1:0] attr_bus;

wire cycle, start, stop;

pu_simple_control #( .MICROCODE_WIDTH( MICROCODE_WIDTH )
                   , .PROGRAM_DUMP( "fibonacci_net/fibonacci_net.dump" )
                   , .MEMORY_SIZE( 8 )
                   ) control_unit
  ( .clk( clk )
  , .rst( rst )
  , .signals_out( control_bus )
  , .cycle( cycle )
  );

wire [DATA_WIDTH-1:0] accum_data_out;
wire [ATTR_WIDTH-1:0] accum_attr_out;

pu_accum 
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 4 )
   ) accum
  ( .clk( clk )
  , .rst( rst )
  , .signal_init( control_bus[18] )
  , .signal_load( control_bus[19] )
  , .signal_neg( control_bus[20] )
  , .signal_oe( control_bus[21] )
  , .data_in( data_bus )
  , .attr_in( attr_bus )
  , .data_out( accum_data_out )
  , .attr_out( accum_attr_out )
  );

wire [DATA_WIDTH-1:0] fram1_data_out;
wire [ATTR_WIDTH-1:0] fram1_attr_out;

pu_fram 
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 4 )
   , .RAM_SIZE( 16 )
   , .FRAM_DUMP( "fibonacci_net/pu_fram.fram1.dump" )
   ) fram1
  ( .clk( clk )
  , .signal_addr( { control_bus[9], control_bus[8], control_bus[7], control_bus[6] } )

  , .signal_wr( control_bus[10] )
  , .data_in( data_bus )
  , .attr_in( attr_bus )

  , .signal_oe( control_bus[11] )
  , .data_out( fram1_data_out )
  , .attr_out( fram1_attr_out )
  );

wire [DATA_WIDTH-1:0] fram2_data_out;
wire [ATTR_WIDTH-1:0] fram2_attr_out;

pu_fram 
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 4 )
   , .RAM_SIZE( 16 )
   , .FRAM_DUMP( "fibonacci_net/pu_fram.fram2.dump" )
   ) fram2
  ( .clk( clk )
  , .signal_addr( { control_bus[3], control_bus[2], control_bus[1], control_bus[0] } )

  , .signal_wr( control_bus[4] )
  , .data_in( data_bus )
  , .attr_in( attr_bus )

  , .signal_oe( control_bus[5] )
  , .data_out( fram2_data_out )
  , .attr_out( fram2_attr_out )
  );

wire [DATA_WIDTH-1:0] shift_data_out;
wire [ATTR_WIDTH-1:0] shift_attr_out;

pu_shift #( .DATA_WIDTH( 32 )
          , .ATTR_WIDTH( 4 )
          ) shift
  ( .clk( clk )
  , .signal_work( control_bus[12] ), .signal_direction( control_bus[13] )
  , .signal_mode( control_bus[14] ), .signal_step( control_bus[15] )
  , .signal_init( control_bus[16] ), .signal_oe( control_bus[17] )
  , .data_in( data_bus )
  , .attr_in( attr_bus )
  , .data_out( shift_data_out )
  , .attr_out( shift_attr_out )
  );

wire [DATA_WIDTH-1:0] spi_data_out;
wire [ATTR_WIDTH-1:0] spi_attr_out;

pu_slave_spi
  #( .DATA_WIDTH( 32 )
   , .ATTR_WIDTH( 4 )
   ) spi
  ( .clk( clk )
  , .rst( rst )
  , .signal_cycle( cycle )
  , .signal_oe( control_bus[23] )
  , .signal_wr( control_bus[22] )
  , .flag_start( start )
  , .flag_stop( stop )
  , .data_in( data_bus )
  , .attr_in( attr_bus )
  , .data_out( spi_data_out )
  , .attr_out( spi_attr_out )
  , .mosi( mosi )
  , .miso( miso )
  , .sclk( sclk )
  , .cs( cs )
  );


assign { attr_bus, data_bus } = 
  { accum_attr_out, accum_data_out } | 
  { fram1_attr_out, fram1_data_out } | 
  { fram2_attr_out, fram2_data_out } | 
  { shift_attr_out, shift_data_out } | 
  { spi_attr_out, spi_data_out } ;

endmodule
