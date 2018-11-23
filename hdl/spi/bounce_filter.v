// FIXME: Для подавления дребезга нужен отдельный модуль. В нем счетчик, который 
// инкрементируется, если на линии 1, и декрементируется, если 0. Счетчик с насыщением. 
// Когда значение доходит до порога, переключается "отфильтрованный" выход. Да вообще 
// это 4 лаба по ИУС))) Там все написано. Необходимо протестировать в железе.
  
module bounce_filter #
        ( parameter DIV = 4
        , parameter CNT = 5 // Учитывается два последних бита
                            // CNT лежит в диапазоне
                            // 2 <= CNT <= 5
        )
    ( input rst
    , input clk
    , input in
    , output out
    );

reg [CNT-1:0] cnt;

always @(posedge clk) begin
    if (rst) begin
        cnt <= 0;
    end else if (in) begin
        if (cnt != {CNT{1'b1}}) cnt <= cnt + 1;
    end else begin
        if (cnt != {CNT{1'b0}}) cnt <= cnt - 1;
    end
end

assign out = cnt[CNT-1] & cnt[CNT-2] ? 1 : 0;
	
endmodule