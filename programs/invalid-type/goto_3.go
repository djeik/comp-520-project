package main

/* Some operators only work on integer types. */

func g() {
  var x = 3.2 << 2 // 3.2 is a float, so not an integer
}
