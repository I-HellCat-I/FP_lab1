from math import factorial

def count_digit_sum(number=factorial(100)):
    cnt = 0
    while number:
        cnt += number % 10
        number //= 10
    return cnt

if __name__ == '__main__':
    print(count_digit_sum())