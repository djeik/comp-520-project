package main

/* Only arrays and slices can be sliced */

func main() {
    var x [10]int
    var t []int
    y := x[2:2:2] //Ok
    z := x[2:2:2] //Ok
    var w int
    u := w[:] // Error here
}
