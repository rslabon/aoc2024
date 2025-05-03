import functools

towels = ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
patterns = [
    "brwrr",
    "bggr",
    "gbbr",
    "rrbgbr",
    "ubwu",
    "bwurrg",
    "brgr",
    "bbrgwb"
]

with open("../../resources/day19.txt") as f:
    block1, block2 = f.read().strip().split("\n\n")

towels = block1.split(", ")
patterns = block2.splitlines()


def find(pattern):
    if len(pattern) == 0:
        return True

    for towel in towels:
        if pattern.startswith(towel):
            new_pattern = pattern[len(towel):]
            if find(new_pattern):
                return True

    return False


def part1():
    count = 0
    for pattern in patterns:
        if find(pattern):
            count += 1
    print(count)


@functools.cache
def count_ways(pattern):
    if len(pattern) == 0:
        return 1
    count = 0
    for towel in towels:
        if pattern.startswith(towel):
            new_pattern = pattern[len(towel):]
            count += count_ways(new_pattern)

    return count


def part2():
    total = 0
    for pattern in patterns:
        total += count_ways(pattern)

    print(total)


part1()  # 236
part2()  # 643685981770598
