def prime_sum_to(n=2000000):
    if n < 2:
        return 0
    if n == 2:
        return 2

    nsum = 2
    for x in range(3, n+1, 2):
        fl = True

        for y in range(3, int(x**0.5)+1):
            if x % y == 0:
                fl = False
                break
        if fl:
            nsum += x

    return nsum

if __name__ == '__main__':
    print(prime_sum_to())