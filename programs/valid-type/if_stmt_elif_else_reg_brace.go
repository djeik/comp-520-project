// true
//.

package main

func main() {
	x, y := true, false
	if z:= true; y {
		println(z)
	} else if z:= true; x {
		println(z)
	} else {
		println(x)
	}
}
