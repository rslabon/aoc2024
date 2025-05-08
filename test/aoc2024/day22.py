def mix(secret, n):
    return secret ^ n


def prune(secret):
    return secret % 16777216


def next_secret(secret):
    secret = mix(secret, secret * 64)
    secret = prune(secret)
    secret = mix(secret, secret // 32)
    secret = prune(secret)
    secret = mix(secret, secret * 2048)
    secret = prune(secret)
    return secret


def secret_at_day(secret, day):
    while day > 0:
        secret = next_secret(secret)
        day -= 1
    return secret


secrets = [1,
           10,
           100,
           2024]

with open("../../resources/day22.txt") as f:
    lines = f.read().strip().splitlines()
    secrets = [int(line) for line in lines]


def part1():
    total = 0
    for secret in secrets:
        total += secret_at_day(secret, 2000)

    print(total)


def part2():
    # secrets = [
    #     1,
    #     2,
    #     3,
    #     2024
    # ]
    global_window_to_first_price = dict()
    for secret in secrets:
        next_secrets = []
        window_to_first_price = dict()
        next_secrets.append(secret)
        day = 2000
        while day > 0:
            secret = next_secret(secret)
            next_secrets.append(secret)
            day -= 1

        i = 1
        changes = []
        prices = []
        while i < len(next_secrets):
            prev = next_secrets[i - 1]
            current = next_secrets[i]
            current_price = int(str(current)[-1])
            prev_price = int(str(prev)[-1])
            change = current_price - prev_price
            prices.append(current_price)
            changes.append(change)
            i += 1

        window = changes[0:4]
        i = len(window)
        while i < len(changes):
            window = window[1:] + [changes[i]]
            i += 1

            if tuple(window) not in window_to_first_price:
                window_to_first_price[tuple(window)] = prices[i - 1]

        for window, price in window_to_first_price.items():
            bananas = global_window_to_first_price.get(window, 0)
            bananas += price
            global_window_to_first_price[window] = bananas

    max_bananas = max(global_window_to_first_price.values())
    print(max_bananas)


part1()  # 16999668565
part2()  # 1898
