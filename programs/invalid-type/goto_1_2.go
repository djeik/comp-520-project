package main

/* Type and variable namespaces are not separate. We need to check that a type
occurs where it is expected, and the reverse. */

func g() {
  var a int
  var b a // Error: a is not a type
}