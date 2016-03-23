package main

/* Complex statements have their own scopes */

func g() {
    for false {
        x := 3
    }

    x = 5 // Error here
}