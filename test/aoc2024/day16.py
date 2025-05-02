import heapq
from collections import deque

with open("../../resources/day16.txt") as f:
    lines = f.read().strip().split("\n")

# lines = """###############
# #.......#....E#
# #.#.###.#.###.#
# #.....#.#...#.#
# #.###.#####.#.#
# #.#.#.......#.#
# #.#.#####.###.#
# #...........#.#
# ###.#.#####.#.#
# #...#.....#.#.#
# #.#.#.###.#.#.#
# #.....#...#.#.#
# #.###.#.#.#.#.#
# #S..#.....#...#
# ###############""".strip().split("\n")


# lines = """#################
# #...#...#...#..E#
# #.#.#.#.#.#.#.#.#
# #.#.#.#...#...#.#
# #.#.#.#.###.#.#.#
# #...#.#.#.....#.#
# #.#.#.#.#.#####.#
# #.#...#.#.#.....#
# #.#.#####.#.###.#
# #.#.#.......#...#
# #.#.###.#####.###
# #.#.#...#.....#.#
# #.#.#.#####.###.#
# #.#.#.........#.#
# #.#.#.#########.#
# #S#.............#
# #################
# """.strip().split("\n")

cells = set()
start = None
end = None
for row, line in enumerate(lines):
    for col, val in enumerate(line):
        if val == "#":
            continue
        cells.add((row, col))
        if val == "S":
            start = (row, col)
        if val == "E":
            end = (row, col)


def adj(cell, dir):
    row, col = cell
    north = (row - 1, col)
    south = (row + 1, col)
    west = (row, col - 1)
    east = (row, col + 1)
    if dir == "SOUTH":
        return [(south, "SOUTH", 1), (west, "WEST", 1001), (east, "EAST", 1001)]
    if dir == "NORTH":
        return [(north, "NORTH", 1), (west, "WEST", 1001), (east, "EAST", 1001)]
    if dir == "EAST":
        return [(east, "EAST", 1), (south, "SOUTH", 1001), (north, "NORTH", 1001)]
    if dir == "WEST":
        return [(west, "WEST", 1), (north, "NORTH", 1001), (south, "SOUTH", 1001)]
    return None


def part1():
    pq = []
    heapq.heappush(pq, (0, start, "EAST"))
    seen = set()

    while pq:
        current_cost, current_point, current_dir = heapq.heappop(pq)
        seen.add((current_point, current_dir))
        if current_point == end:
            print(current_cost)
            break

        for next_point, next_dir, next_cost in adj(current_point, current_dir):
            if next_point not in cells:
                continue
            if (next_point, next_dir) in seen:
                continue

            heapq.heappush(pq, (current_cost + next_cost, next_point, next_dir))


def part2():
    pq = []
    heapq.heappush(pq, (0, start, "EAST", None, None, None))
    lowest_cost = {(start, "EAST"): 0}
    prev = {}
    best_cost = float("inf")
    end_state = None

    while pq:
        current_cost, current_point, current_dir, prev_cost, prev_point, prev_dir = heapq.heappop(pq)
        if current_cost > lowest_cost.get((current_point, current_dir), float("inf")):
            continue
        lowest_cost[(current_point, current_dir)] = current_cost

        if current_point == end:
            if current_cost > best_cost:
                break

            best_cost = current_cost
            end_state = (current_point, current_dir)

        if (current_point, current_dir) not in prev:
            prev[(current_point, current_dir)] = set()

        prev[(current_point, current_dir)].add((prev_point, prev_dir))

        for next_point, next_dir, next_cost in adj(current_point, current_dir):
            if next_point not in cells:
                continue
            lowest = lowest_cost.get((next_point, next_dir), float("inf"))
            if next_cost > lowest:
                continue
            heapq.heappush(pq,
                           (current_cost + next_cost, next_point, next_dir, current_cost, current_point, current_dir))

    q = deque()
    q.append(end_state)
    seen = set()
    seen.add(end_state)
    while q:
        state = q.popleft()
        for prev_state in prev.get(state, []):
            if prev_state in seen:
                continue

            seen.add(prev_state)
            q.append(prev_state)

    points = set(map(lambda item: item[0], seen))
    # for row, line in enumerate(lines):
    #     for col, val in enumerate(line):
    #         if (row, col) in points:
    #             print("o", end="")
    #         else:
    #             print(val, end="")
    #     print()

    print(len(points) - 1)


part1()
part2()
