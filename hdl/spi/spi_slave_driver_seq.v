`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Simple SPI master controller with CPOL=0, CPHA=1
//////////////////////////////////////////////////////////////////////////////////

module spi_slave_driver2 #
        ( parameter DATA_WIDTH = 8
        )
    ( input            clk
    , input            rst
    // system interface
    , input  [DATA_WIDTH-1:0] data_in  // data that master can read from slave
    , output reg              ready    // transaction is not processed now 
    , output                  prepare
    , output [DATA_WIDTH-1:0] data_out // data written to slave in last transaction
    // SPI iterface
    , output reg       miso
    , input            mosi
    , input            sclk
    , input            cs
    );

// В связи с тем, что "прокачать" все данные по комбинационной схеме не удаётся на нужной частоте,
// необходимо сделать это заранее. Это может быть реализовано за счёт использования нескольких 
// сдвиговых регистров, где один используется для загрузки данных "на будущее", а второй в работе.
reg shiftreg_sel;
reg [DATA_WIDTH-1:0] shiftreg [0:1];
wire [DATA_WIDTH-1:0] shiftreg0 = shiftreg[0];
wire [DATA_WIDTH-1:0] shiftreg1 = shiftreg[1];
wire work = shiftreg_sel;
wire load = !shiftreg_sel;
reg [$clog2( DATA_WIDTH + 1 )-1:0] counter; // 0 - ничего не передано.
                                            // 1..n - сейчас передаётся
                                            // n+1 - пошли на круг.

localparam STATE_IDLE = 0; // wait for transaction begin
localparam STATE_WAIT_CS_0 = 1;
localparam STATE_WAIT_SCLK_1 = 2; // wait for SCLK to become 1
localparam STATE_WAIT_SCLK_0 = 3; // wait for SCLK to become 0
reg   [2:0] state;

always @( posedge clk ) begin
    if ( rst ) begin
        shiftreg_sel <= 0;
        shiftreg[0] <= 0;
        shiftreg[1] <= 0;
        counter <= 0;
        miso <= 0;
        ready <= 1;
        state <= STATE_IDLE;
    end else begin
        shiftreg[load] <= data_in;
        if ( state == STATE_IDLE ) begin
            if ( !cs ) begin
                counter <= 0;
                shiftreg_sel <= !shiftreg_sel;
                state <= STATE_WAIT_SCLK_1;
                ready <= 0;
            end
        end else if ( state == STATE_WAIT_SCLK_1 ) begin
            ready <= 0;
            if ( sclk ) begin
                miso <= shiftreg[work][ DATA_WIDTH - 1 ];
                counter <= counter + 1;  
                state <= STATE_WAIT_SCLK_0;
            end else if ( cs ) begin
                counter <= DATA_WIDTH + 1;
                state <= STATE_IDLE;
            end
        end else if ( state == STATE_WAIT_SCLK_0 ) begin
            if ( !sclk ) begin
                if ( counter == DATA_WIDTH ) begin
                    shiftreg_sel <= !shiftreg_sel;
                    ready <= 1;
                    counter <= 0;
                end
                shiftreg[work] <= { shiftreg[work][DATA_WIDTH - 2:0], mosi };
                state <= STATE_WAIT_SCLK_1;
            end
        end else begin
            state <= STATE_IDLE;
        end
    end
end

assign prepare = state == STATE_WAIT_SCLK_1 && counter + 1 == DATA_WIDTH && sclk;
assign data_out = shiftreg[work];
  
endmodule
