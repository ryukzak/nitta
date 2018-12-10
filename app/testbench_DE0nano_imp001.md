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

# Схема подключение интерфейса SPI к imp001

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
Start Compilation@. Затем вам необходимо прошить в ПЛИС результат. Для этого выбираем пункт
@Tools -> Programmer@. В поле File должен быть указан путь до прошивки nitta.sof, если в поле 
написано <none>, нужно два раза нажать на это поле и выбрать прошивку по 
пути output_files/nitta.sof, затем установить checkbox Program/Configure в активное состояние.
Прошиваем результат в ПЛИС нажав на кнопку @Start@. Если кнопка неактивна:

- проверьте подключение платы DE0-Nano;
- нажмите @Hardware Setup@ и выберите @USB-Blaster@;
- обратитесь к руководству пользователя отладочной платы:
  <http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=165&No=593&PartNo=4>.

Убедитесь, что положение дип переключателей соответствует указанному на схеме выше.

Теперь подготовьте к работе управляющий контроллер. Для этого вам необходимо:

Инструкции если используется аккаунт @aleksandpenskoi@:

1. Поднять Wi-Fi сеть и подключить к ней контроллер в соответствии с инструкцией
   <https://developer.electricimp.com/gettingstarted/explorer/blinkup>.
2. В случае если вы используете аккаунт @aleksandpenskoi@ зайдите в строке @SPI_testbench@ выберите
   @Development Zone@, затем @Code@.
3. Убедитесь, что последней вызываемой функцией в правой части экрана (Device Code) является
   функция, одноименная названию демо.
4. Внизу слева вы увидете список подключённых к проекту устройств. Напротив нужного нажмите кнопку с иконкой On/Off.
5. В терминале снизу справа должен будет появиться журнал передачи данных на подобие приведённого
   ниже.

Инструкции если используется новосозданный аккаунт @new_account@:

1. Поднять Wi-Fi сеть и подключить к ней контроллер в соответствии с инструкцией
   <https://developer.electricimp.com/gettingstarted/explorer/blinkup>.
2. Зарегистрировать акаунт на стайте electric imp
   <https://impcentral.electricimp.com>.
3. Слева в меню  выбрать вкладку @Development Device Group Devices@ и нажать на кнопку @Assign@. 
   В пункте Device Group указать Development Device Group и название DDG которые вы давали 
   при создании проекта, после нажать на @Assign Devices@. Должно появиться поле с DEVICE ID со 
   статусом online.
4. Слева в меню переходим во вкладку @Code@ и в поле Device Code вставляем код расположенный ниже:

```
sc <- hardware.pin8;
sc.configure(DIGITAL_OUT, 1);

spi1 <- hardware.spi257;
spi1.configure( CLOCK_IDLE_LOW | CLOCK_2ND_EDGE, 1000 )

function echoTest_2bytes() {
    local tmp;
    local i = 0;   
    
    while ( i <= 0xFF) {
        local b = blob(2);
        b.writen( i, 'c' );
        b.writen( i + 1, 'c' );
        sc.write(0);
        tmp = spi1.writeread(b);
        sc.write(1);
        i += 2;
    }
}

function fibonacciTest(n) {
    local tmp;
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(8);
        sc.write(0);
        tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;
        server.log(tmp)
    }
}

function getTest(n, size) {
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(size);
        sc.write(0);
        local tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;

        server.log(tmp);
        tmp.swap4();
        local a = tmp.readn('i');
        local b = tmp.readn('i');
        server.log( format("> %d\t%d", a, b) );
    }
}

function fibonacciDemo(n) {
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(8);
        sc.write(0);
        local tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;

        server.log(tmp);
        tmp.swap4();
        local a = tmp.readn('i');
        local b = tmp.readn('i');
        server.log( format("> Номер: %d\tЗначение:%d", b, a) );
    }
}

function teacupDemo(n) {
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(8);
        sc.write(0);
        local tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;

        server.log(tmp);
        tmp.swap4();
        local a = tmp.readn('i');
        local b = tmp.readn('i');
        server.log( format("> Номер: %d\tЗначение:%d", b, a) );
    }
}

// getTest(20, 8)
// getTest(4, 12)
// getTest(4, 8)
// getTest(4, 12)
// getTest(10, 4)

// echoTest_2bytes()
// echoTest_4bytes()

// fibonacciTest(10)

fibonacciDemo(10)
```

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

Аналогичным образом будет выглядеть работу других демо.
