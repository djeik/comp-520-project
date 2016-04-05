package main

var (
    // Because of the font's aspect ratio, we don't render a square but
    // rather a rectangle.
    H, W = 512, 1024
    N_ITERATIONS = 1024
    MAX_BOUND = 1000000.
)

var t [70]string; // Table converting from intensity to an actual character
func initt() {
    // GoLite supports neither array literals nor string indexing :(
    // Characters taken from http://paulbourke.net/dataformats/asciiart/
    t[0] = " "; t[1] = "."; t[2] = "'"; t[3] = "`"; t[4] = "^"; t[5] = "\"";
    t[6] = ","; t[7] = ":"; t[8] = ";"; t[9] = "I"; t[10] = "l"; t[11] = "!";
    t[12] = "i"; t[13] = ">"; t[14] = "<"; t[15] = "~"; t[16] = "+"; t[17] = "_";
    t[18] = "-"; t[19] = "?"; t[20] = "]"; t[21] = "["; t[22] = "}"; t[23] = "{";
    t[24] = "1"; t[25] = ")"; t[26] = "("; t[27] = "|"; t[28] = "\\"; t[29] = "/";
    t[30] = "t"; t[31] = "f"; t[32] = "j"; t[33] = "r"; t[34] = "x"; t[35] = "n";
    t[36] = "u"; t[37] = "v"; t[38] = "c"; t[39] = "z"; t[40] = "X"; t[41] = "Y";
    t[42] = "U"; t[43] = "J"; t[44] = "C"; t[45] = "L"; t[46] = "Q"; t[47] = "0";
    t[48] = "O"; t[49] = "Z"; t[50] = "m"; t[51] = "w"; t[52] = "q"; t[53] = "p";
    t[54] = "d"; t[55] = "b"; t[56] = "k"; t[57] = "h"; t[58] = "a"; t[59] = "o";
    t[60] = "*"; t[61] = "#"; t[62] = "M"; t[63] = "W"; t[64] = "&"; t[65] = "8";
    t[66] = "%"; t[67] = "B"; t[68] = "@"; t[69] = "$";

}


func juliaIterate(a, b float64) int {
    var x, y float64 = 0., 0.
    i := 0
    for ; i < N_ITERATIONS ; i++ {
        if (x*x + y*y > MAX_BOUND) {
            //println(i, " ", x*x + y*y)
            break
        }

        x1 := x*x - y*y + a
        y1 := 2.*x*y + b

        x, y = x1, y1
    }

    return i
}

func mbrot(aMin, aMax, bMin, bMax float64) {
    for i := 0 ; i < H ; i++ {
        for j := 0 ; j < W ; j++ {
            var a = (aMax - aMin) * float64(j) / float64(W) + aMin
            var b = (bMax - bMin) * float64(i) / float64(H) + bMin

            nit := juliaIterate(a,b)
            clamped := int(70. * float64(nit) / 256.)
            if clamped > 69 {
                clamped = 69
            }

            print(t[clamped])
        }

        println()
    }
}

func main() {
    initt()

    // Same spot as https://en.wikipedia.org/wiki/File:Mandelbrot_sequence_new.gif
    var ac, bc =   -0.743643887037158704752191506114774,
                    0.131825904205311970493132056385139

    z := 1.5

    for i := 1 ; i <= 8 ; i++ {
        z /= float64(i)
        mbrot(ac - z, ac + z, bc - z, bc + z)
        println()
    }

}
