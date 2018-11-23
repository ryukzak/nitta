// FIXME: Для подавления дребезга нужен отдельный модуль. В нем счетчик, который 
// инкрементируется, если на линии 1, и декрементируется, если 0. Счетчик с насыщением. 
// Когда значение доходит до порога, переключается "отфильтрованный" выход. Да вообще 
// это 4 лаба по ИУС))) Там все написано. Необходимо протестировать в железе.
  
module bounce_filter #
        ( parameter DIV = 5
        )
    ( input rst
    , input clk
    , input in
    , output out
    );


reg [DIV-1:0] cnt;

generate
    if ( DIV == 0 || DIV == 1 ) begin
        assign out = in;
    end else begin
        assign out = cnt[DIV-1] & cnt[DIV-2] ? 1 : 0;
        always @(posedge clk) begin
            if (rst) begin
                cnt <= 0;
            end else if (in) begin
                if (cnt != {DIV{1'b1}}) cnt <= cnt + 1;
            end else begin
                if (cnt != {DIV{1'b0}}) cnt <= cnt - 1;
            end
        end        
    end
endgenerate
	
endmodule