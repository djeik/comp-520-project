/* Fibonacci program with semi-colon terminators. */
package main ;

func fibonacci(n int) int { if n < 2 { return n ;
	} ;
	return fibonacci(n - 1) + fibonacci(n - 2) ;
} ;


var m int = 10 ;
func main() { println(fibonacci(m)) ;
	} ;
