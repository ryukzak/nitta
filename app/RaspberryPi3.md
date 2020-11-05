# Raspberry Pi 3 configuration

```
(black)      GND ----------------------+
(white)       CS ------------------+   |
                 +-----------------|---|-----------------------+
                 |  +--------------|---|-------+     +---------+
                 |  |ooooo  ooooo  *ooo*  ooooo|     |         |
                 |  |ooooo  oooo*  **ooo  ooooo|     |         |
                 |  +-----------|--||----------+     +---------+
(orange)    MOSI ---------------+  ||                +---------+
(blue)      MISO ------------------+|                |         |
(yellow)    SCLK -------------------+                |         |
                 |                                   +---------+
                 |                                 +-----------+
                 |                                 |           |
                 |  +-----+                        |           |
                 |  | USB |                        +-----------+
                 +--+-----+------------------------------------+
```

For SSH connection on Windows we recommended to use [PuTTY](https://www.putty.org/).

1. Run PuTTY and fill the following fields:
   - Host Name: 192.168.1.78
   - Port: 22
   - Connection type: SSH
   - Click 'Open'
2. Default login and password:
   - login: pi
   - password: raspberry
3. `> cd NITTA/c-spi/`
4. `> make && make start`
``` console
$ make && make start
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

**ATTENTION!** After work with RPi you need to execute:
```console
$ sudo shutdown -h now
```