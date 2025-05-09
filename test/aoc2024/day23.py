import networkx as nx

connections = [
    "kh-tc",
    "qp-kh",
    "de-cg",
    "ka-co",
    "yn-aq",
    "qp-ub",
    "cg-tb",
    "vc-aq",
    "tb-ka",
    "wh-tc",
    "yn-cg",
    "kh-ub",
    "ta-co",
    "de-co",
    "tc-td",
    "tb-wq",
    "wh-td",
    "ta-ka",
    "td-qp",
    "aq-cg",
    "wq-ub",
    "ub-vc",
    "de-ta",
    "wq-aq",
    "wq-vc",
    "wh-yn",
    "ka-de",
    "kh-ta",
    "co-tc",
    "wh-qp",
    "tb-vc",
    "td-yn",
]

with open("../../resources/day23.txt") as f:
    connections = f.read().strip().splitlines()

network = dict()
for connection in connections:
    c1, c2 = connection.split("-")
    line = network.get(c1, set())
    line.add(c2)
    network[c1] = line
    line = network.get(c2, set())
    line.add(c1)
    network[c2] = line


def is_connected(c, others):
    result = True
    for o in others:
        result = result and c in network[o]

    return result


def part1():
    lines = set()
    for c1, line in network.items():
        for c2 in line:
            for c3 in network[c2]:
                if c1 != c3 and is_connected(c1, [c2, c3]) and is_connected(c2, [c1, c3]) and is_connected(c3,
                                                                                                           [c1, c2]):
                    if c1[0] == 't' or c2[0] == 't' or c3[0] == 't':
                        lines.add(tuple(sorted([c1, c2, c3])))

    print(len(lines))


def part2():
    G = nx.Graph()
    for connection in connections:
        c1, c2 = connection.split("-")
        G.add_edge(c1, c2)

    cliques = nx.find_cliques(G)
    largest_clique = max(cliques, key=len)
    print(",".join(sorted(largest_clique)))


# part1()
part2()
