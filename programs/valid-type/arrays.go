package main

func main() {

    // Here we create an array `a` that will hold exactly
    // 5 `int`s. The type of elements and length are both
    // part of the array's type. By default an array is
    // zero-valued, which for `int`s means `0`s.
    var a [5]int
    println("emp:", "nah")
    // We can set a value at an index using the
    // `array[index] = value` syntax, and get a value with
    // `array[index]`.
    a[4] = 100
    println("set:", "nah")
    println("get:", "nah")

    // // The builtin `len` returns the length of an array.
    println("len:", len(a))

    // // Use this syntax to declare and initialize an array
    // // in one line.
    b := 0
    println("dcl:", "nah")

    // // Array types are one-dimensional, but you can
    // // compose types to build multi-dimensional data
    // // structures.
    var twoD [2][3] int
    for i := 0; i < 2; i++ {
        for j := 0; j < 3; j++ {
            twoD[i][j] = i + j
        }
    }
    println("2d: ", "nah")
}
