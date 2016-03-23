package main

/* Redeclaration is supported in short var decls, given that at least one
non-blank new variable is also declared. */

func g() {
  x, y := 1, 2
  x, z := 3, 4 // Allowed, z is a new variable.
  x := 1 // Error: no new variable here
}
