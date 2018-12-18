# Настройка Raspberry Pi 3

**Pins**

name signal | pin 
:-- | :--:  
mosi | 19 
miso | 21 
sclk | 23 
cs | 22 

```
(черный)     GND ----------------------+
(белый)       CS ------------------+   |
                 +-----------------|---|-----------------------+
                 |  +--------------|---|-------+     +---------+
                 |  |ooooo  ooooo  *ooo*  ooooo|     |         |
                 |  |ooooo  oooo*  **ooo  ooooo|     |         |
                 |  +-----------|--||----------+     +---------+
(оранжевый) MOSI ---------------+  ||                +---------+
(синий)     MISO ------------------+|                |         |
(жёлтый)    SCLK -------------------+                |         |
                 |                                   +---------+
                 |                                 +-----------+
                 |                                 |           |
                 |  +-----+                        |           |
                 |  | USB |                        +-----------+
                 +--+-----+------------------------------------+
```

Инструкции далее предполагают уже подготовленное De-nano к работе. 

Для работы с RPi3 нам понадобиться программа [PuTTY](https://www.putty.org/) 
для подключения по SSH.

1. Откройте PuTTY и в соответствующие поля добавьте:
   - Host Name: 192.168.1.78
   - Port: 22
   - Connection type: SSH
   - Нажмите Open
2. RPi потребует логин и пароль:
   - login: pi
   - password: raspberry
3. Перейдите в директорию с исходниками:
   ```>> cd NITTA/c-spi/```
4. Выполните команду сборки и запуска программы:
   ```>> make && make start``` 
5. В терминале снизу справа должен будет появиться журнал передачи данных на 
   подобие приведённого ниже.

```
pi@raspberrypi:~/NITTA/c-spi $ make && make start
gcc -o spi spi.c -l bcm2835
sudo ./spi
Read back from SPI: 0x00 0x00 0x00 0x00 | 00000, Number: 0x00 0x00 0x00 0x00
Read back from SPI: 0x00 0x00 0x00 0x01 | 00001, Number: 0x00 0x00 0x00 0x01
Read back from SPI: 0x00 0x00 0x00 0x01 | 00001, Number: 0x00 0x00 0x00 0x02
Read back from SPI: 0x00 0x00 0x00 0x02 | 00002, Number: 0x00 0x00 0x00 0x03
Read back from SPI: 0x00 0x00 0x00 0x03 | 00003, Number: 0x00 0x00 0x00 0x04
Read back from SPI: 0x00 0x00 0x00 0x05 | 00005, Number: 0x00 0x00 0x00 0x05
Read back from SPI: 0x00 0x00 0x00 0x08 | 00008, Number: 0x00 0x00 0x00 0x06
Read back from SPI: 0x00 0x00 0x00 0x0D | 00013, Number: 0x00 0x00 0x00 0x07
Read back from SPI: 0x00 0x00 0x00 0x15 | 00021, Number: 0x00 0x00 0x00 0x08
Read back from SPI: 0x00 0x00 0x00 0x22 | 00034, Number: 0x00 0x00 0x00 0x09
```

**ОБЯЗАТЕЛЬНО!** При завершении работы с RPi, нужно выполнить команду:

```>> sudo shutdown -h now```