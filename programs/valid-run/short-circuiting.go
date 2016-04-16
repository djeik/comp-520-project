// true!
// hello
// falsy!
// true after all!
// falsy!
// this is an else clause
// hi
// falsy!
// done
//.

package test

func falsy() bool {
	println("falsy!")
	return false
}

func truthy() bool {
	println("truthy!")
	return true
}

func main() {
	if true || falsy() {
		println("true!")
	}

	println("hello")

	if falsy() || true {
		println("true after all!")
	}

	if true && falsy() {
		println("no way!")
	} else {
		println("this is an else clause")
	}

	println("hi")

	if falsy() && true {
		println("nop")
	}

	println("done")
}
