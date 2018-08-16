# SPI dataflow

```
                    Target System                               Tests
                                                                    
I/O            | miso, mosi, sclk, cs           |              |  |  |
               \--------------------------------/              |  |  |
                   |     |     |    |                          |  |  |
               /--------------------------------\              |  |  |
               | bounce_filter.v                |              |  |  |
               |    ||                          |              |  |  |
pu_slave_spi.v | pu_slave_spi_driver.v          |   echo_test  |  |  |
               |    ||                          |  ------------/  |  |
               | nitta_to_spi_splitter.v        |   splitter_test |  |
               |    ||                          |  ---------------/  |
               | buffer.v                       |   buffer_test      |
               \--------------------------------/  ------------------/
                    ||
               /--------------------------------
NITTA          | data_in, data_out, signals
```

## echo_test
`iverilog -o pu_slave_spi_driver_tb.out pu_slave_spi_driver.v pu_slave_spi_driver_tb.v && vvp.exe pu_slave_spi_driver_tb.out`
