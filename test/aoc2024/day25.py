from collections import Counter


def is_fit(lock_heights, key_heights):
    i = 0
    while i < len(lock_heights):
        if lock_heights[i] + key_heights[i] > 5:
            return False
        i += 1

    return True


def parse_heights(s):
    lines = s.strip().splitlines()

    trim_bottom_and_top = lines[1:-1]
    rotated = [[0] * len(trim_bottom_and_top) for _ in range(len(lines[0]))]
    for row, line in enumerate(trim_bottom_and_top):
        for col, val in enumerate(line):
            rotated[col][row] = val

    heights = []
    for row in rotated:
        heights.append(Counter(row).get("#", 0))

    if set(lines[0]) == {'#'}:
        return "lock", heights

    return "key", heights


with open("../../resources/day25.txt") as f:
    blocks = f.read().strip().split("\n\n")

locks = []
keys = []
for block in blocks:
    type, heights = parse_heights(block)
    if type == "lock":
        locks.append(heights)
    else:
        keys.append(heights)


def part1():
    count = 0
    for lock in locks:
        for key in keys:
            if is_fit(lock, key):
                count += 1

    print(count)


part1()
