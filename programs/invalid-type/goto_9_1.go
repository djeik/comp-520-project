package main

/* Complex statements have their own scopes */

func g() {
    if true {
        x := 3
    }

    x = 5 // Error here
}