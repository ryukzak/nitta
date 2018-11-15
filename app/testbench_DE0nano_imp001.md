# Test bench organisation
Test bench was designed with the following components:

1.  The tool computer (imp001). It provides control of a
    modelling process and data transmission. There is using a single-board computer by Electric imp.
2.  A NITTA processor. It evaluates system dynamics model in the real-time and provides modelling
    data by its interfaces as inputs for a system under test. For realising this component DE-0 Nano
    board with Cyclone IV FPGA have been used.

The controller and the NITTA processor communicate throughout SPI (for data transmission)
interfaces. Those interfaces were chosen because they are ubiquitous.

A user and the test bench communicate throughout electricimp cloud-based IDE
<https://impcentral.electricimp.com>.

```
+---------------------------------------------+
| Browser: https://impcentral.electricimp.com |
+---------------------------------------------+
    |
    |
    |
+----------+
| Internet |
+----------+
    |
    | Wi-Fi (old version, maybe you need to use Wi-Fi tethering)
    |
+--------+   SPI   +----------+
| imp001 |---------| DE0-nano |
+--------+         +----------+
```

# Схема подключение интерфейса SPI к DE0-nano

В скобках указан рекомендованный цвет провода. Пропорции нарушены.

```
           +--------------------------------------- GND (черный)
+----------|---------------------+
|    ooooo *oooo ooooo ooooo     |
|    ooooo ooooo ooooo o****     |
|                       |||+------------------------ CS   (белый)
+-----+                 ||+------------------------- SCLK (жёлтый)
| USB |                 |+-------------------------- MISO (синий)
+-----+                 +--------------------------- MOSI (оранжевый)
|                    +--------+  |
|  +--------------+  | ALTERA |  |
|  |        =     |  +--------+  |
|  | DIP: 4 3 2 1 |              |
|  |      =   =   |              |
|  +------|-|-|---+              |
|         | | +----- rendevou (down)
|         | +------ frequency (up)
|         +-------------- rst (down)
|                                |
|                                |
|    ooooo ooooo ooooo ooooo     |
|    ooooo ooooo ooooo ooooo     |
+--------------------------------+
```

= Схема подключение интерфейса SPI к imp001

```
                    +--------------------------------+
                    | o GND                          |
                    | o VIN                          |
                    | o                              |
                    | o Pin1                         |
(синий)     MISO------* Pin2           imp001        |
(жёлтый)    SCLK------* Pin5                         |
(оранжевый) MOSI------* Pin7                         |
(белый)       CS -----* Pin8                         |
                    | o Dtc9                         |
                    | o 3V3   +-----+                |
(черный)     GND -----* GND   | USB |                |
                    +---------+-----+----------------+
```

# Требуемоемое программное обеспечение

Для запуска и наладки испытательного стенда необходимо следующее программное обеспечение:

1. Проект [САПР NITTA]<https://nitta.io/nitta-corp/nitta>, включая реализацию данного модуля.
2. [haskell stack]<https://nitta.io/penskoi/nitta/src/master/doc/stack-install.md>, необходим для
   сборки САПР и генерации демонстрационного проекта.
3. [Quartus Prime Lite Edition]<https://fpgasoftware.intel.com/?edition=lite> and Cyclone IV device
   support. It's needed for synthesis and uploading a demo project into DE0-nano evaluation board.

Also, you need an account on <https://impcentral.electricimp.com> for getting data from the NITTA
processor. You can manually register and creating the project for that purpose or request login and
password from the project maintainer.

# Запуск демо

У вас на столе лежит собранный и подключённый стенд, на компьютере установлено всё необходимо
программное обеспечение и вы открыли консоль на соответсвующем каталоге. Дальнейшее описание
приведено для Windows 10, но также должно быть справедливо для Linux.

Каждое демо представляет из себя функцию, выполнение которой приведёт к генерации проекта, который
можно будет синтезировать, загрузить в испытательный стенд и проверить работоспособность. Её
выполнение приведет к генерации одноименного каталога с проектом в директории @/hdl/gen/DEMO_NAME@.
Для начала работы с демо необходимо либо вставить вызов функции в функцию 'Main.main', либо
осуществить запуск из @stack repl@.

```
>>> :m Demo
>>> fibonacciDemo
Demo project in hdl/gen/fibonacciDemo
```

Откройте сгенерированный проект в @Quartus@ (файл @nitta.qpf@) и синтезируйте его: @Processing ->
Start Compilation@. Затем вам необходимо прошить в ПЛИС результат. Для этого @Tools -> Programmer@,
затем кнопка @Start@. Если кнопка неактивна:

- проверьте подключение платы DE0-Nano;
- нажмите @Hardware Setup@ и выберите @USB-Blaster@;
- обратитесь к руководству пользователя отладочной платы:
  <http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=165&No=593&PartNo=4>.

Убедитесь, что положение дип переключателей соответствует указанному на схеме выше.

Теперь подготовьте к работе управляющий конроллер. Для этого вам необходимо:

1. Поднять Wi-Fi сеть и подключить к ней контроллер в соответствии с инструкцией
   <https://developer.electricimp.com/gettingstarted/explorer/blinkup>.
2. В случае если вы используете аккаунт @aleksandpenskoi@ зайдите в строке @SPI_testbench@ выберите
   @Development Zone@, затем @Code@.
3. Убедитесь, что последней вызываемой функцией в правой части экрана (Device Code) является
   функция, одноименная названию демо.
4. Внизу слева вы увидете список подключённых к проекту устройств. Напротив нужного нажмите кнопку с
   иконкой On/Off.
5. В терминале снизу справа должен будет появиться журнал передачи данных на подобие приведённого
   ниже.

```
2018-08-20 16:19:27 +03:00 	[Status] 	Agent restarted: reload.
2018-08-20 16:19:28 +03:00 	[Status] 	Device connected
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 00 00 00 00 00
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 0	Значение: 0
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 01 00 00 00 01
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 1	Значение: 1
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 01 00 00 00 02
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 2	Значение: 1
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 02 00 00 00 03
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 3	Значение: 2
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 03 00 00 00 04
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 4	Значение: 3
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 05 00 00 00 05
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 5	Значение: 5
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 08 00 00 00 06
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 6	Значение: 8
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 0d 00 00 00 07
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 7	Значение: 13
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 15 00 00 00 08
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 8	Значение: 21
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 22 00 00 00 09
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 9	Значение: 34
```

Аналогичным образом выглядят будет выглядеть работу других демо.