package main

var world_width = 80
var world_height = 24

type grid [24][80]int

var w = 0
var worlds [2]grid

func at(world grid, x int, y int) bool {
	return (world[y][x] > 0)
}

func inc(b bool) int {
	if b {
		return 1
	} else {
		return 0
	}
}

func count_neighbours(world grid, x int, y int) int {
	count := 0

	for i := -1; i < 2; i += 2 {
		if x_ := x + i; x_ >= 0 && x_ < world_width {
			for j := -1; j < 2; j++ {
				if y_ := y + j; y_ >= 0 && y_ < world_height {
					d := inc(at(world, x_, y_))
					if d > 0 {
						//println(d, "neighbor at", x_, y_)
					}
					count += d
				}
			}
		}
	}

	if y-1 >= 0 {
		d := inc(at(world, x, y-1))
		if d > 0 {
			//println("neighour at", x, y-1)
		}
		count += d
	}

	if y+1 < world_height {
		d := inc(at(world, x, y+1))
		if d > 0 {
			//println("neighbour at", x, y+1)
		}
		count += d
	}

	if count > 0 {
		//println("count", count, "at", x, y)
	}

	return count
}

func update() {
	for i := 0; i < world_width; i++ {
		for j := 0; j < world_height; j++ {
			worlds[(w+1)%2][j][i] = worlds[w][j][i]
			if nc := count_neighbours(worlds[w], i, j); nc == 3 {
				//println("birth at", i, j)
				worlds[(w+1)%2][j][i] += 1
			} else if nc == 2 && at(worlds[w], i, j) {
				//println("survive at", i, j)
				worlds[(w+1)%2][j][i] += 1
			} else {
				if nc > 0 {
					//println("die at", i, j)
				}
				worlds[(w+1)%2][j][i] = 0
			}
		}
	}
}

func draw_world() {
	world := worlds[w]

	for j := 0; j < world_height; j++ {
		for i := 0; i < world_width; i++ {
			if t := at(world, i, j); t {
				print("#")
			} else {
				print(" ")
			}
		}
		print("\n")
	}

	for i := 0; i < world_width; i++ {
		print("-")
	}
	print("\n")
}

func main() {

	worlds[0][1][25] = 1
	worlds[0][2][23] = 1
	worlds[0][2][25] = 1
	worlds[0][3][13] = 1
	worlds[0][3][14] = 1
	worlds[0][3][21] = 1
	worlds[0][3][22] = 1
	worlds[0][3][35] = 1
	worlds[0][3][36] = 1
	worlds[0][4][12] = 1
	worlds[0][4][16] = 1
	worlds[0][4][21] = 1
	worlds[0][4][22] = 1
	worlds[0][4][35] = 1
	worlds[0][4][36] = 1
	worlds[0][5][1] = 1
	worlds[0][5][2] = 1
	worlds[0][5][11] = 1
	worlds[0][5][17] = 1
	worlds[0][5][21] = 1
	worlds[0][5][22] = 1
	worlds[0][6][1] = 1
	worlds[0][6][2] = 1
	worlds[0][6][11] = 1
	worlds[0][6][15] = 1
	worlds[0][6][17] = 1
	worlds[0][6][18] = 1
	worlds[0][6][23] = 1
	worlds[0][6][25] = 1
	worlds[0][7][11] = 1
	worlds[0][7][17] = 1
	worlds[0][7][25] = 1
	worlds[0][8][12] = 1
	worlds[0][8][16] = 1
	worlds[0][9][13] = 1
	worlds[0][9][14] = 1

	for i := 0; i < 1000; i, w = i+1, (w+1)%2 {
		draw_world()
		update()
	}
}
