package main

import (
	"fmt"
	"math"
)

type MyInt int64

const N int64 = 16 // 64 - 48
const MyN MyInt = MyInt(N)

func Fix(x float64) MyInt {
	return MyInt(x * (1 << N))
}

func Unfix(x MyInt) float64 {
	return float64(x) / (1 << N)
}

func MulF(xs ...MyInt) MyInt {
	res := xs[0]
	for _, x := range xs[1:] {
		res = (res * x) >> N
	}
	return res
}

func DivF(a MyInt, b MyInt) MyInt {
	return (a / b) << N
}

func main() {
	var generatorF float64 = 50
	T := Fix(0.001)

	alpha := Fix(0.7)
	beta := Fix(5)
	gamma := Fix(80)
	// r := Fix(1)
	rPow2 := Fix(1)
	omegaN := Fix(125.66)              // Fix(2 * 3.1415 * 20)
	pi2 := Fix(6.28318)                // Fix(3.14159 * 2)
	omegaNDIVPi2 := Fix(19.9994270417) // Fix((2 * 3.1415 * 20) / (3.14159 * 2))

	var x [4]MyInt
	signal := make([]MyInt, 300)
	for i := range signal {
		signal[i] = Fix(math.Sin(2 * 3.14 * generatorF * float64(i) * Unfix(T)))
		// fmt.Printf("%08x \n", int64(signal[i]))
		// fmt.Printf("%f %f %f %f %f %f %f \n", Unfix(alpha), Unfix(beta), Unfix(gamma), Unfix(rPow2), Unfix(omegaN), Unfix(pi2), Unfix(omegaNDIVPi2))
	}
	fmt.Println()
	// return

	var tmp1, tmp2, a, b, c, dotX0, dotX1, dotX2, dotX3, omega MyInt
	for _, u := range signal {

		tmp1 = u - x[1] - x[3]
		tmp2 = MulF(x[0], x[0]) + MulF(x[1], x[1]) - rPow2

		omega = omegaN + MulF(pi2, x[2])

		a = MulF(alpha, tmp1, omega)
		b = MulF(x[0], omega)
		c = MulF(x[1], tmp2)

		dotX0 = MulF(x[1], omega)
		dotX1 = a - b - c
		dotX2 = MulF(Fix(-1), omega, x[0], beta, tmp1)
		dotX3 = MulF(gamma, tmp1)

		// fmt.Printf("%f\t%f\n", Unfix(a), Unfix(tmp1))
		// fmt.Printf("%f\t%f\t%f\t%f\n", Unfix(a), Unfix(u), Unfix(x[1]), Unfix(x[3]))

		x[0] = x[0] + MulF(dotX0, T)
		x[1] = x[1] + MulF(dotX1, T)
		x[2] = x[2] + MulF(dotX2, T)
		x[3] = x[3] + MulF(dotX3, T)

		freq := omegaNDIVPi2 + x[2]
		// fmt.Printf("%f\t%f\t%f\t%f\n", Unfix(dotX0), Unfix(dotX1), Unfix(dotX2), Unfix(dotX3))
		// fmt.Printf("%f\t%f\t%f\n", Unfix(a), Unfix(b), Unfix(c))
		fmt.Printf("%f\n", Unfix(freq))
	}
}
