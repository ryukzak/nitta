// TODO: Данный файл тоже должен генерироваться автоматически при построении testbench-а.
module net_tb();
  reg clk, rst;
  accum_fram1_fram2_net net(
    .clk(clk),
    .rst(rst)
    );

  initial begin
    clk = 1'b0;
    rst = 1'b1;
    repeat(4) #10 clk = ~clk;
    rst = 1'b0;
    forever #10 clk = ~clk;
  end

  initial
    begin
      $dumpfile("net_tb.vcd");
      $dumpvars(0, net_tb);

      @(negedge rst);                
      forever @(posedge clk); 
    end

  initial
    begin
      // program_counter == 1
      // на шину управление выставлены значения соответсвующие адресу 0 в памяти пока не снят rst
      @(negedge rst); // Влючение процессора. 
      // Сразу после снятия сигнала rst на шину управления выставляются сигналы соответствующие адресу 1.
      // После следующего положительного фронта будет получен результат.      
      `include "hdl/gen/accum_fram1_fram2_net_assertions.v"
      $finish;
    end

endmodule
