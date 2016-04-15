pat = """
........................O
......................O.O
............OO......OO............OO
...........O...O....OO............OO
OO........O.....O...OO
OO........O...O.OO....O.O
..........O.....O.......O
...........O...O
............OO
"""

VAR_NAME = 'worlds[0]'

if __name__ == '__main__':
    x = 0
    y = 0
    for c in pat:
        case = lambda x: x == c

        if case('.'):
            x += 1
        elif case('\n'):
            y += 1
            x = 0
        elif case('O'):
            x += 1
            print('%s[%d][%d] = 1' % (VAR_NAME, y, x))
