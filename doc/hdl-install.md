# Инструментарий для моделирования/тестирования аппаратуры (Icarus Verilog)

Для автоматизированного тестирования в проекте используется Icarus Verilog
<http://iverilog.icarus.com>. 

Для просмотра временных диаграмма рекомендуется использовать gtkwave.

Для синтеза и ручного тестирования вычислительных блоков используется Quartus и
ModelSim. Бесплатные версии могут быть скачаны с сайта после регистрации
<https://www.altera.com/downloads/download-center.html>.

Если при запуске `nitta-exe` или тестов возникает ошибка: `nitta-exe.EXE: iverilog: readCreateProcessWithExitCode: does not exist (No such file or directory)`, то это связано с некоректным значением переменной окружения PATH.