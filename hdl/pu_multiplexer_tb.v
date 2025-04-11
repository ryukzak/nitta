module pu_multiplexer_tb();

    parameter DATA_WIDTH = 32;
    parameter ATTR_WIDTH = 4;
    parameter SEL_WIDTH = 1;

    reg clk;
    reg data_active;
    reg sel_active;
    reg out_active;
    reg [DATA_WIDTH-1:0] data_in;
    reg [ATTR_WIDTH-1:0] attr_in;
    wire [DATA_WIDTH-1:0] data_out;
    wire [ATTR_WIDTH-1:0] attr_out;

    pu_multiplexer #(
        .DATA_WIDTH(DATA_WIDTH),
        .ATTR_WIDTH(ATTR_WIDTH),
        .SEL_WIDTH(SEL_WIDTH))
    dut (
        .clk(clk),
        .data_active(data_active),
        .sel_active(sel_active),
        .out_active(out_active),
        .data_in(data_in),
        .attr_in(attr_in),
        .data_out(data_out),
        .attr_out(attr_out)
    );

    // Тактовый генератор
    initial begin
        clk = 0;
        forever #1 clk = ~clk;
    end

    task automatic write_data;
        input [SEL_WIDTH-1:0] sel;
        input [DATA_WIDTH-1:0] data;
        input [ATTR_WIDTH-1:0] attr;
        begin
            @(posedge clk);
            sel_active <= 1;
            data_in <= sel;
            @(posedge clk);
            sel_active <= 0;
            
            data_active <= 1;
            data_in <= data;
            attr_in <= attr;
            @(posedge clk);
            data_active <= 0;
            $display("[%0t] Write: sel=%0d, data=0x%h, attr=0x%h", $time, sel, data, attr);
        end
    endtask

    task automatic check_output;
        input [SEL_WIDTH-1:0] expected_sel;
        input [DATA_WIDTH-1:0] expected_data;
        input [ATTR_WIDTH-1:0] expected_attr;
        begin
            @(posedge clk);
            out_active <= 1;
            @(posedge clk);
            if (data_out !== expected_data || attr_out !== expected_attr)
                $display("FAIL: sel=%0d, expected (0x%h, 0x%h), got (0x%h, 0x%h)",
                        expected_sel, expected_data, expected_attr, data_out, attr_out);
            else
                $display("PASS: sel=%0d, data=0x%h, attr=0x%h",
                        expected_sel, data_out, attr_out);
            out_active <= 0;
            @(posedge clk);
        end
    endtask

    initial begin
        $dumpfile("pu_multiplexer_tb.vcd");
        $dumpvars(0, pu_multiplexer_tb);

        // Инициализация
        data_active = 0;
        sel_active = 0;
        out_active = 0;
        data_in = 0;
        attr_in = 0;
        
        // Тест 1: Запись и чтение разных слотов
        write_data(0, 32'hAAAA_AAAA, 4'hA);
        write_data(1, 32'h5555_5555, 4'h5);
        
        // Проверка выбора слота 0
        write_data(0, 0, 0); // Изменяем селектор
        check_output(0, 32'hAAAA_AAAA, 4'hA);
        
        // Проверка выбора слота 1
        write_data(1, 0, 0); // Изменяем селектор
        check_output(1, 32'h5555_5555, 4'h5);

        // Тест 2: Проверка сброса выхода
        @(posedge clk);
        out_active <= 0;
        @(posedge clk);
        if (data_out !== 0 || attr_out !== 0)
            $display("FAIL: Output not reset");
        else
            $display("PASS: Output reset");

        // Тест 3: Перезапись данных
        write_data(0, 32'hDEAD_BEEF, 4'hF);
        check_output(0, 32'hDEAD_BEEF, 4'hF);

        #100;
        $finish;
    end

endmodule
