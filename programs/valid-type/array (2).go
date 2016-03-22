// go lecture (6), slide 8
package main

var ( a = 5; i = 1; )

func main () {
	var x [5] int
	var y[][10] int
	x[i] = 5
	x[i], a = 1,2
	y[x[5]][0] = 0
	x[5] = 0
}
