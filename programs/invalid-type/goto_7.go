package main

/* Thens and Elses are in different scopes. */

func main() {
    if x := 2; true {
        y:= 2
    } else {
        print(y) //Error here
    }
}