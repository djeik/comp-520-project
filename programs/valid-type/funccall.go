package funcall

func a() {}
func v() int { return 2; }
func f(_,_,_ int) {}

func main() {
    a()
    ((((v))))()
    f(1,3,4)
}
