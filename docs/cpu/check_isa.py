#!/bin/python
from typing import Sequence

inst_codes = []

def get_inst_len(inst_code):
    field_d = (inst_code >> 12) & 0xf
    field_c = (inst_code >>  8) & 0xf
    field_b = (inst_code >>  4) & 0xf
    field_a = (inst_code >>  0) & 0xf
    field_e_needed = field_d == 0xf or \
        (field_c == 0xf and (field_b != 0xf or field_a == 0xf)) or \
        (field_c == 0xe and (field_a == 0xf)) or \
        (field_c  < 0xc and (field_b == 0xf or field_a == 0xf))
    field_e_is_16 = field_d == 0xf or field_a != 0xf
    if not field_e_needed: return 16
    if field_e_is_16: return 32
    return 48

for inst_code in range(0x10000):
    inst_codes.append(None)

def get_digits(pattern: str) -> Sequence[int]:
    if pattern in ("0123456789abcdefABCDEF"): return (int(pattern, base=16), )
    if pattern == ".": return (0,1,2,3,4,5,6,7,8,9,0xa,0xb,0xc,0xd,0xe,)
    if pattern == "*": return (0,1,2,3,4,5,6,7,8,9,0xa,0xb,0xc,0xd,0xe,0xf)
    assert False

with open("isa.rst", "rt") as file:
    while file:
        line = file.readline()
        if line == "":
            break
        if line.endswith("\n"): line = line[:-1]
        if "see below" in line: continue
        start = 2
        if line.startswith("EXT"):
            start += 4
        if line[start-2:].startswith("0x"):
            inst_code = line[start:start+4]
            inst_len = 16
            try:
                if line[start+5:].startswith("0x"): inst_len += 16
                if line[start+12:].startswith("0x"): inst_len += 16
            except IndexError:
                pass
            for digit1 in get_digits(inst_code[0]):
                for digit2 in get_digits(inst_code[1]):
                    for digit3 in get_digits(inst_code[2]):
                        for digit4 in get_digits(inst_code[3]):
                            inst_code_int = digit1 << 12 | digit2 << 8 | digit3 << 4 | digit4
                            #print(f"0x{inst_code_int:04x}", end=" ")
                            if inst_codes[inst_code_int] is not None:
                                print(f"!!!!!!!!!!!!!!!! CONFLICT !!!!!!!!!!!!!!!!")
                                print(f"between:")
                                print(f"    {inst_codes[inst_code_int]}")
                                print(f"    {line}")
                                print(f"    at inst code: 0x{inst_code_int:04x}")
                            if inst_len != get_inst_len(inst_code_int):
                                print(f"!!!!!!!!!!!!!!!! SIZE CONFLICT !!!!!!!!!!!!!!!!")
                                print(f"between:")
                                print(f"    {inst_codes[inst_code_int]}")
                                print(f"    {line}")
                                print(f"    should be {get_inst_len(inst_code_int)} bits long")
                            inst_codes[inst_code_int] = line
            #print(line)
            pass

print("Code map:")
for inst_code, line in enumerate(inst_codes):
    if line is not None:
        print(f"    0x{inst_code:04x} - {line}")

# Try to collate groups of unused codes

print("Unused codes:")
#for inst_code, line in enumerate(inst_codes):
#    if line is None:
#        print(f"    0x{inst_code:04x}")

# Try to collate groups of unused codes

from silicon import OrderedSet
unused_codes_16 = OrderedSet(f"{inst_code:04x}" for inst_code, line in enumerate(inst_codes) if line is None and get_inst_len(inst_code) == 16)
unused_codes_32 = OrderedSet(f"{inst_code:04x}" for inst_code, line in enumerate(inst_codes) if line is None and get_inst_len(inst_code) == 32)
unused_codes_48 = OrderedSet(f"{inst_code:04x}" for inst_code, line in enumerate(inst_codes) if line is None and get_inst_len(inst_code) == 48)

def compact_codes(unused_codes: OrderedSet):
    changes = True
    while changes:
        changes = False
        for inst_code in unused_codes:
            def replace_digit(code, loc, value):
                if isinstance(value, int): value = hex(value)[2:]
                l = list(code)
                l[loc] = value
                return "".join(l)

            # Let's see if we can find any '.' or '*' that this thing is a part of
            pattern = "."
            for d in range(4):
                found = True
                for digit in get_digits(pattern):
                    new_inst_code = replace_digit(inst_code, d, digit)
                    if new_inst_code not in unused_codes:
                        found = False
                        break
                if found:
                    pattern_code = replace_digit(inst_code, d, pattern)
                    # Mark all chosen entries as used
                    for digit in get_digits(pattern):
                        new_inst_code = replace_digit(inst_code, d, digit)
                        unused_codes.remove(new_inst_code)
                    unused_codes.add(pattern_code)
                    changes = True
                    break
            if changes: break

compact_codes(unused_codes_16)
compact_codes(unused_codes_32)
compact_codes(unused_codes_48)

for code in unused_codes_16:
    print(f"    0x{code}")

for code in unused_codes_32:
    print(f"    0x{code} 0x****")

for code in unused_codes_48:
    print(f"    0x{code} 0x**** 0x****")
