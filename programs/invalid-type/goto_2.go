package main

/* Field naming matters in structs. Two struct types can only be identical if
their field names are the same. */

func g() {
  var x struct { a, b int; }
  var y struct { b, a int; }
  z := x == y // Error: the types are not compatible
}
