package main

func main() {
    x := 1

    {
        print(x)
        x := 2
        print(x)
    }

    print(x)
}
