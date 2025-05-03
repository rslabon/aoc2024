import heapq

corrupted_bytes = []
with open("../../resources/day18.txt") as f:
    lines = f.read().strip().splitlines()
for line in lines:
    x, y = line.split(",")
    x = int(x)
    y = int(y)
    corrupted_bytes.append((x, y))


def part1(corrupted_bytes):
    width = 71
    height = 71
    limit = 1024
    corrupted_bytes = set(corrupted_bytes[0:limit])

    start = (0, 0)
    end = (width - 1, height - 1)
    pq = []
    heapq.heappush(pq, (0, start))
    seen = set()

    while pq:
        cost, (cx, cy) = heapq.heappop(pq)
        if (cx, cy) in seen: continue
        seen.add((cx, cy))
        if (cx, cy) == end:
            print(cost)

        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            nx = cx + dx
            ny = cy + dy
            if not (width > nx >= 0 <= ny < height): continue
            if (nx, ny) in corrupted_bytes: continue
            if (nx, ny) in seen: continue
            heapq.heappush(pq, (cost + 1, (nx, ny)))


def part2(corrupted_bytes):
    width = 71
    height = 71
    limit = 1024
    while limit < len(corrupted_bytes):
        found_exit = False
        blocked = set(corrupted_bytes[0:limit])

        start = (0, 0)
        end = (width - 1, height - 1)
        pq = []
        heapq.heappush(pq, (0, start))
        seen = set()

        while pq:
            cost, (cx, cy) = heapq.heappop(pq)
            if (cx, cy) in seen: continue
            seen.add((cx, cy))
            if (cx, cy) == end:
                found_exit = True

            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                nx = cx + dx
                ny = cy + dy
                if not (width > nx >= 0 <= ny < height): continue
                if (nx, ny) in blocked: continue
                if (nx, ny) in seen: continue
                heapq.heappush(pq, (cost + 1, (nx, ny)))

        if not found_exit:
            print(corrupted_bytes[limit - 1])
            return

        limit += 1


part1(corrupted_bytes)  # 330
part2(corrupted_bytes) # 10,38

# limit = 3014
# blocked = set(corrupted_bytes[0:limit])
# for y in range(0, 71):
#     for x in range(0, 71):
#         if (x, y) in blocked:
#             print("#", end="")
#         else:
#             print(".", end="")
#     print()
