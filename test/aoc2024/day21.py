import functools
import re
from collections import deque

codes = [
    "671A",
    "826A",
    "670A",
    "085A",
    "283A"
]

directional_keypad = [
    ["#", "^", "A"],
    ["<", "v", ">"],
]

directional_key_to_xy = dict()
for row, line in enumerate(directional_keypad):
    for col, key in enumerate(line):
        directional_key_to_xy[key] = (row, col)

numeric_keypad = [
    ["7", "8", "9"],
    ["4", "5", "6"],
    ["1", "2", "3"],
    ["#", "0", "A"]
]

numeric_key_to_xy = dict()
for row, line in enumerate(numeric_keypad):
    for col, key in enumerate(line):
        numeric_key_to_xy[key] = (row, col)


@functools.cache
def push_directional_button(current, target, level=0):
    q = deque()
    q.append((directional_key_to_xy[current], [], set()))
    min_length = float("inf")

    while q:
        (x, y), path, seen = q.popleft()
        if directional_keypad[x][y] == target:
            if level == 1:
                min_length = min(min_length, len("".join(path) + "A"))
            else:
                min_length = min(min_length, push_directional_sequence("".join(path) + "A", level - 1))

        for dx, dy, dir in [(-1, 0, "^"), (1, 0, "v"), (0, 1, ">"), (0, -1, "<")]:
            nx, ny = x + dx, y + dy
            if not (0 <= nx < len(directional_keypad) and 0 <= ny < len(directional_keypad[0])): continue
            if (nx, ny) in seen: continue
            if directional_keypad[nx][ny] == "#": continue

            q.append(((nx, ny), path + [dir], seen | {(nx, ny)}))

    return min_length


def push_numeric_button(current, target):
    q = deque()
    q.append((numeric_key_to_xy[current], [], set()))
    paths = []

    while q:
        (x, y), path, seen = q.popleft()
        if numeric_keypad[x][y] == target:
            paths.append("".join(path) + "A")

        for dx, dy, dir in [(-1, 0, "^"), (1, 0, "v"), (0, 1, ">"), (0, -1, "<")]:
            nx, ny = x + dx, y + dy
            if not (0 <= nx < len(numeric_keypad) and 0 <= ny < len(numeric_keypad[0])): continue
            if (nx, ny) in seen: continue
            if numeric_keypad[nx][ny] == "#": continue

            q.append(((nx, ny), path + [dir], seen | {(nx, ny)}))

    return paths


@functools.cache
def push_directional_sequence(sequence, level=0):
    current = "A"
    length = 0
    for target in sequence:
        length += push_directional_button(current, target, level)
        current = target

    return length


def take_min(paths):
    min_path = min(paths, key=len)
    return [p for p in paths if len(p) == len(min_path)]


def push_numeric_sequence(sequence):
    current = "A"
    paths = []
    for target in sequence:
        paths.append(take_min(push_numeric_button(current, target)))
        current = target

    q = deque(paths)
    result = []
    while q:
        a = q.popleft()
        if not q:
            result = a
            break
        b = q.popleft()
        q.append([x + y for x in a for y in b])

    return result


def find_complexities(robots):
    total = 0
    for code in codes:
        min_length = float("inf")
        for s in push_numeric_sequence(code):
            min_length = min(min_length, push_directional_sequence(s, robots))

        number = int(re.findall(r"\d+", code)[0])
        total += number * min_length

    print(total)


def part1():
    find_complexities(2)


def part2():
    find_complexities(25)


part1()
part2()
