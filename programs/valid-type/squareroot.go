package main

func sqrt(x float64) float64 {
	var quot float64
	guess := 1.0

	for iter := 10; iter > 0; iter-- {
		quot = x / guess
		guess = 0.5 * (guess + quot)
	}
	return guess
}

func main() {
}
