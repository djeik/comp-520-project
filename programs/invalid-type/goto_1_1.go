package main

/* Type and variable namespaces are not separate. We need to check that a type
occurs where it is expected, and the reverse. */

func g() {
  var a int
  {
    type a int
    a = 2 // Error: a is not a value
  }
}
