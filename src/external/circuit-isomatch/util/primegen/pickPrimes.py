#!/usr/bin/env python3

from genprime import gen_one as gen_one_prime
from random import randint

# CONSTANTS
operations = [
    'and',
    'or',
    'xor',
    'add',
    'sub',
    'mul',
    'div',
    'mod',
    'lsl',
    'lsr',
    'asr',
    'not',
    'un_lsr',
    'un_lsl',
    'un_asr',
    'cstint',
    'wireid',
    'numconst',
    'longconst',
    'merge',
    'slice',
    'slicebounds',
    'leaftype',
    'groupIO',
]

PRIME_MAGNITUDE = 10**8


print("// ==== CONSTANTS ==== (Auto-generated)")
print("// ===================")
for op in operations:
    print("const OperConstants opcst_{}({}u, {}u, {}u, {}u, {}u);".format(
        op,
        randint(1 << 16, 1 << 32),
        gen_one_prime(PRIME_MAGNITUDE),
        gen_one_prime(PRIME_MAGNITUDE),
        gen_one_prime(PRIME_MAGNITUDE),
        gen_one_prime(PRIME_MAGNITUDE)))
