puzzle = """
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
"""

with open("../../resources/day24.txt") as f:
    puzzle = f.read().strip()

block1, block2 = puzzle.split("\n\n")
block1_lines = block1.strip().splitlines()
block2_lines = block2.strip().splitlines()

wires = dict()
for line in block1_lines:
    wire, value = line.split(": ")
    value = int(value)
    wires[wire] = value

connections = dict()
for line in block2_lines:
    left, output = line.split(" -> ")
    input1, gate, input2 = left.split(" ")
    connections[output] = (input1, gate, input2)


def evaluate_wire(wire):
    if wire in wires:
        return wires[wire]
    input1, gate, input2 = connections[wire]
    if gate == "AND":
        value = evaluate_wire(input1) & evaluate_wire(input2)
    elif gate == "OR":
        value = evaluate_wire(input1) | evaluate_wire(input2)
    elif gate == "XOR":
        value = evaluate_wire(input1) ^ evaluate_wire(input2)
    else:
        raise ValueError(f"Unknown gate: {gate}")
    wires[wire] = value
    return value


def part1():
    for wire in connections:
        evaluate_wire(wire)
    zwires = sorted([wire for wire in wires if wire.startswith("z")])
    values = reversed([str(wires[wire]) for wire in zwires])
    print(int("".join(values), 2))


part1()