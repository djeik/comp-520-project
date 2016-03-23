package main

/* Cases have their own scopes */

func g() {
    switch {
        case true:
            x := 2
        case false:
            x = 5 // error here
    }
}
