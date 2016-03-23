package main

/* Redeclaration is supported in short var decls, given that at least one
non-blank new variable is also declared. But of course any redeclared variables
need to have the same type! */

func g() {
  x := 1
  x, y := "string", 2 // Invalid here!
}
