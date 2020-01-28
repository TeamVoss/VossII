from random import randint

''' This base makes Miller-Rabin deterministic for numbers below 2^64
    (see <http://miller-rabin.appspot.com/>)
'''
MR_BASES = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]


def expmod(base, exp, mod):
    if exp == 1:
        return base

    out = expmod((base * base) % mod, exp >> 1, mod)
    if exp & 1:
        return out * base
    return out


def miller_rabin(num, pow2, rem, base):
    x = expmod(base, rem, num)
    if x == 1 or x == num - 1:
        return True

    for i in range(pow2 - 1):
        x = (x * x) % num
        if x == 1:
            return False
        elif x == num - 1:
            return True
    return False


def is_prime(num):
    pow2 = 0
    rem = num - 1
    while rem & 1 == 0:
        pow2 += 1
        rem >>= 1

    for base in MR_BASES:
        if not miller_rabin(num, pow2, rem, base):
            return False
        return True


def gen_one_around(around):
    while not is_prime(around):
        around += 1
    return around


def gen_one(magnitude, magnitude_range=10):
    return gen_one_around(randint(magnitude, magnitude * magnitude_range))
