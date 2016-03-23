package main

/* Redeclaration is supported in short var decls, given that at least one
non-blank new variable is also declared. */

func g() {
  x, y := 1, 2
  x, _ := 3, 4 // Error: no new NON_BLANK variable here
}
