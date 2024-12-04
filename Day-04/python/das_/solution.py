grid = open("input.txt").read().splitlines()

def part1(row, column, direction, length=0):
    return (length == 4 or
            (0 <= row < len(grid) and
             0 <= column < len(grid[0]) and
             grid[row][column] == "XMAS"[length] and
             part1(row + direction[0], column + direction[1], direction, length + 1)))

def part2(row, column):
    word1 = grid[row - 1][column - 1] + grid[row + 1][column + 1]
    word2 = grid[row + 1][column - 1] + grid[row - 1][column + 1]
    return 1 if word1 in {"MS", "SM"} and word2 in {"MS", "SM"} else 0

print(sum(part1(i, j, d) for i in range(len(grid)) for j in range(len(grid[0])) if grid[i][j] == "X" for d in [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]))
print(sum(part2(i, j) for i in range(1, len(grid) - 1) for j in range(1, len(grid[0]) - 1) if grid[i][j] == "A"))