package main

/* Complex statements have their own scopes */

func g() {
    switch {
        default: x := 3
    }

    x = 5 // Error here
}