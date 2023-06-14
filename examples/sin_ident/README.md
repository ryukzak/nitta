# Periodic Signal Frequency Identification

A reference implementation of the sin_ident algorithm on golang in fixed point.

> go run main.go

Building by NITTA:

> ➜  stack exec nitta -- examples/sin_ident/sin_ident.lua -lf -t fx48.64 -v -n 350

H. Ahmed, M. Bierhoff and M. Benbouzid, "Multiple Nonlinear Harmonic Oscillator-Based Frequency Estimation for Distorted Grid Voltage," in IEEE Transactions on Instrumentation and Measurement, vol. 69, no. 6, pp. 2817-2825, June 2020, doi: 10.1109/TIM.2019.2931065.
