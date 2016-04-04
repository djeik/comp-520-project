/* This code is mostly adapted from PixelMachine by SuperJer
(http://www.superjer.com/pixelmachine) */

package raytrace

type (
    vec_t struct { x, y, z float64; }
    intensity_t int

    rayRes_t struct {
        intens intensity_t // An intensity
        mult float64 // A multiplier to another intensity
    }
)

/* ================== GENERAL LIBRARY FUNCTIONS ================== */

var EPSILON = 0.000001

// xorshift random generator implementation from wikipedia, since we don't have
// a standard library.
var seed int //Must be initialized to a non-zero value
func rand() int {
    seed ^= seed >> 12
    seed ^= seed << 25
    seed ^= seed >> 27
    return seed * 2685821657736338717
}

// Newton approximation for square roots
func sqrt(n float64) float64 {
    var x = 1.

    for i := 0 ; i < 8 ; i++ {
        x = x - (x * x - n) / (2 * x)
    }

    return x
}

func abs(n float64) float64 {
    if n < 0 {
        return -n
    } else {
        return n
    }
}

// ===== Vector functions ==

func vadd(a, b vec_t) vec_t {
    var r vec_t
    r.x = a.x + b.x
    r.y = a.y + b.y
    r.z = a.z + b.z
    return r
}

func vsub(a, b vec_t) vec_t {
    var r vec_t
    r.x = a.x - b.x
    r.y = a.y - b.y
    r.z = a.z - b.z
    return r
}

func vcross(a, b vec_t) vec_t {
    var r vec_t
    r.x = a.y*b.z - a.z*b.y
    r.y = a.z*b.x - a.x*b.z
    r.z = a.x*b.y - a.y*b.x
    return r
}

func vscale(a vec_t, n float64) vec_t {
    var r vec_t = a
    r.x *= n
    r.y *= n
    r.z *= n
    return r
}

func vnorm(a vec_t, n float64) vec_t {
    l := sqrt(a.x * a.x  +  a.y * a.y  +  a.z * a.z)

    var r vec_t = a
    r.x *= n/l;
    r.y *= n/l;
    r.z *= n/l;
    return r
}

/* ================== END OF LIBRARY FUNCTIONS ================== */

var CAM, LOOKAT vec_t

var NUM_INTENSITIES = 70

var TER_SZ = 16
var BLK_SZ = 1000.0 / float64(16)

// Dimensions: x, y, z
var blocks [16][16][16]intensity_t

var OUT_H, OUT_W = 50, 200

var XY, XZ, YZ = 1, 2, 3

func raytrace(cam, ray vec_t) intensity_t {
    p := 0.
    k := 0
    side := XY

    var c intensity_t

    for {
        var blk_x = int( (cam.x + p * ray.x) / BLK_SZ)
        var blk_y = int( (cam.y + p * ray.y) / BLK_SZ)
        var blk_z = int( (cam.z + p * ray.z) / BLK_SZ)

        if  blk_x < 1 || blk_x > TER_SZ - 1 ||
                blk_y < 1 || blk_y > TER_SZ - 1 ||
                blk_z < 0 || blk_z > TER_SZ - 1 || k > 1000 {

            c = intensity_t(0)
            break
        }

        k++

        c = blocks[blk_x][blk_y][blk_z]


        if (c != intensity_t(0)) {
            var sideFactor float64
            if side == XY {
                sideFactor = 1.
            } else if side == XZ {
                sideFactor = .9
            } else {
                sideFactor = .8
            }

            c = intensity_t(float64(c) * sideFactor)
            break
        }

        var hit_x = float64(blk_x) * BLK_SZ
        if ray.x > 0. {
            hit_x += BLK_SZ
        }

        var hit_y = float64(blk_y) * BLK_SZ
        if ray.y > 0. {
            hit_y += BLK_SZ
        }

        var hit_z = float64(blk_z) * BLK_SZ
        if ray.z > 0. {
            hit_z += BLK_SZ
        }

        p0 := (hit_x - cam.x) / ray.x + 0.00001
        p1 := (hit_y - cam.y) / ray.y + 0.00001
        p2 := (hit_z - cam.z) / ray.z + 0.00001

        if  int((cam.x+p0*ray.x)/BLK_SZ) == blk_x &&
                int((cam.y+p0*ray.y)/BLK_SZ) == blk_y &&
                int((cam.z+p0*ray.z)/BLK_SZ) == blk_z {
            p0 = 99999.0
        }
        if  int((cam.x+p1*ray.x)/BLK_SZ)==blk_x &&
                int((cam.y+p1*ray.y)/BLK_SZ) == blk_y &&
                int((cam.z+p1*ray.z)/BLK_SZ) == blk_z {
            p1 = 99999.0
        }

        if  int((cam.x+p2*ray.x)/BLK_SZ) == blk_x &&
                int((cam.y+p2*ray.y)/BLK_SZ) == blk_y &&
                int((cam.z+p2*ray.z)/BLK_SZ) == blk_z {
            p2 = 99999.0
        }

        if p0 <p || p1 < p || p2 < p {
            break
        }

        if p0 < p1 && p0 < p2 {
            side = YZ
            p = p0
        } else if p1 < p2 {
            side = XZ
            p = p1
        } else {
            side = XY
            p = p2
        }
    }

    if float64(c) < 0. || float64(c) > float64(NUM_INTENSITIES)

    return c
}

func render(w, h int) [][]intensity_t {

    // Bring the lookat to 1 unit of the cam.
    LOOKAT = vsub(LOOKAT, CAM)
    LOOKAT = vnorm(LOOKAT, 1.)
    LOOKAT = vadd(LOOKAT, CAM)

    var pixpitch = 2. / float64(w)
    var subpitch = pixpitch / 2.

    var line = vsub(LOOKAT, CAM)

    var img [][]intensity_t

    for i := 0 ; i < w ; i++ {

        var row []intensity_t

        for j := 0 ; j < h ; j++ {
            var di, dj = float64(i), float64(j)

            var wing vec_t
            wing.x = line.x
            wing.y = -line.x
            wing.z = 0.

            var head = vcross(line, wing)

            wing = vnorm(wing, (di - float64(w) / 2.) * pixpitch + subpitch)
            wing = vnorm(wing, (dj - float64(h) / 2.) * pixpitch + subpitch)

            var ray = vsub((vadd(vadd(LOOKAT, wing), head)), CAM)

            row = append(row, raytrace(CAM, ray))
        }

        img = append(img, row)
    }

    return img
}

var t [70]rune; // Table converting from intensity to an actual character
func init() {
    // GoLite supports neither array literals nor string indexing :(
    // Characters taken from http://paulbourke.net/dataformats/asciiart/
    t[0] = ' '; t[1] = '.'; t[2] = '\''; t[3] = '`'; t[4] = '^'; t[5] = '"';
    t[6] = ','; t[7] = ':'; t[8] = ';'; t[9] = 'I'; t[10] = 'l'; t[11] = '!';
    t[12] = 'i'; t[13] = '>'; t[14] = '<'; t[15] = '~'; t[16] = '+'; t[17] = '_';
    t[18] = '-'; t[19] = '?'; t[20] = ']'; t[21] = '['; t[22] = '}'; t[23] = '{';
    t[24] = '1'; t[25] = ')'; t[26] = '('; t[27] = '|'; t[28] = '\\'; t[29] = '/';
    t[30] = 't'; t[31] = 'f'; t[32] = 'j'; t[33] = 'r'; t[34] = 'x'; t[35] = 'n';
    t[36] = 'u'; t[37] = 'v'; t[38] = 'c'; t[39] = 'z'; t[40] = 'X'; t[41] = 'Y';
    t[42] = 'U'; t[43] = 'J'; t[44] = 'C'; t[45] = 'L'; t[46] = 'Q'; t[47] = '0';
    t[48] = 'O'; t[49] = 'Z'; t[50] = 'm'; t[51] = 'w'; t[52] = 'q'; t[53] = 'p';
    t[54] = 'd'; t[55] = 'b'; t[56] = 'k'; t[57] = 'h'; t[58] = 'a'; t[59] = 'o';
    t[60] = '*'; t[61] = '#'; t[62] = 'M'; t[63] = 'W'; t[64] = '&'; t[65] = '8';
    t[66] = '%'; t[67] = 'B'; t[68] = '@'; t[69] = '$';

    seed = 2349872

    cam.x = 500.
    cam.y = 1000.
    cam.z = 500.

    lookat.x = 500.
    lookat.y = 999.
    lookat.z 500.
}

func main () {
    init()

    // Floor
    for x := 0 ; x < TER_SZ ; x++ {
        for y := 0 ; y < TER_SZ ; y++ {
            var i intensity_t
            if (x + y) & 1 == 0 {
                i = intensity_t(1)
            } else {
                i = intensity_t(68)
            }

            blocks[0][x][y] = i
        }
    }

    /*for x := 0 ; x < TER_SZ ; x++ {
        for y := 0 ; y < TER_SZ ; y++ {
            h := rand() % (TER_SZ / 2)
            for z := 0 ; z < h z++ {
                blocks[x][y][z + 1] = rand() % NUM_INTENSITIES
            }
        }
    }*/

    var result = render(W, H)
    for y := 0 ; y < H ; y++ {
        for x := 0 ; x < W ; x++ {
            print(result[y][x])
        }
        println()
    }
}
