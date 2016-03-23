package main

/* Two alias types are the same if and only if they are declared at the same
point in the source code. Therefore, in the following code, x and y have
different types and thus cannot be compared, even though the names and
underlying types of the types are the same.

Note that this is true to the spec, as seen in the second sentence of
https://golang.org/ref/spec#Type_identity */

type a int

func g() {
  var x a
  {
    type a int
    var y a
    z := x == y //Error occurs here
  }
}
