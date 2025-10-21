
def cound_alp_nums(filename='names.txt'):
    nsum = 0
    with open(filename) as f:
        names = sorted(map(lambda x: x.strip('"'), (','.join(f.readlines())).split(',')))
        for i, x in enumerate(names):
            nsum += sum(map(lambda y: ord(y) - ord('A') + 1, list(x))) * (i + 1)
    return nsum

if __name__ == '__main__':
    print(cound_alp_nums())