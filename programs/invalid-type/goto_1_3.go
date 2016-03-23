package main

/* Type and variable namespaces are not separate. We need to check that a type
occurs where it is expected, and the reverse. It also holds for short var decls,
obviously :)*/

func g() {
  type a int
  a, b := 1, 2 // Error: a is a type
}
