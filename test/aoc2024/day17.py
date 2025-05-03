import random


def part1(program, register_A, register_B, register_C):
    instruction_pointer = 0

    def combo(operand):
        if 0 <= operand <= 3:
            return operand
        if operand == 4:
            return register_A
        if operand == 5:
            return register_B
        if operand == 6:
            return register_C
        raise "ERRRR"

    out = []
    while instruction_pointer < len(program):
        opcode = program[instruction_pointer]
        operand = program[instruction_pointer + 1]

        if opcode == 0:
            register_A = int(register_A // 2 ** combo(operand))
        elif opcode == 1:
            register_B = register_B ^ operand
        elif opcode == 2:
            register_B = combo(operand) % 8
        elif opcode == 3:
            if register_A == 0:
                instruction_pointer += 2
                continue
            instruction_pointer = operand
            continue
        elif opcode == 4:
            register_B = register_B ^ register_C
        elif opcode == 5:
            out.append(combo(operand) % 8)
        elif opcode == 6:
            register_B = int(register_A // 2 ** combo(operand))
        elif opcode == 7:
            register_C = int(register_A // 2 ** combo(operand))

        instruction_pointer += 2
        # print(f"A={register_A} B={register_B} C={register_C}")

    return out


print(part1([2, 4, 1, 6, 7, 5, 4, 4, 1, 7, 0, 3, 5, 5, 3, 0], 37293246, 0, 0))
