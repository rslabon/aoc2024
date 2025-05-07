import heapq

lines = """###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############""".strip().splitlines()

with open("../../resources/day20.txt") as f:
    lines = f.read().strip().splitlines()

blocked = set()
open = set()
start = None
end = None
for y, line in enumerate(lines):
    for x, val in enumerate(line):
        if val == ".":
            open.add((x, y))
        if val == "#":
            blocked.add((x, y))
        if val == "S":
            start = (x, y)
            open.add((x, y))
        if val == "E":
            end = (x, y)
            open.add((x, y))


def race(cheat=None):
    pq = []
    heapq.heappush(pq, (0, start))
    seen = set()
    cheat_open = set(open)
    if cheat:
        cheat_open.add(cheat)

    while pq:
        cost, (cx, cy) = heapq.heappop(pq)
        if (cx, cy) in seen: continue
        seen.add((cx, cy))

        if (cx, cy) == end:
            return cost

        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            nx = cx + dx
            ny = cy + dy
            if (nx, ny) not in cheat_open: continue
            if (nx, ny) in seen: continue
            heapq.heappush(pq, (cost + 1, (nx, ny)))

    return float("inf")


def part1():
    normal = race()
    score = {}
    for cheat in blocked:
        saved = normal - race(cheat)
        if saved < 100: continue
        score[saved] = score.get(saved, 0) + 1

    total = 0
    for saved, count in score.items():
        total += count

    print(total)


part1() # 1343