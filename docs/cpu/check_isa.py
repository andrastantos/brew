###!/usr/bin/python
from typing import Sequence, Union

def get_inst_len(inst_code):
    field_d = (inst_code >> 12) & 0xf
    field_c = (inst_code >>  8) & 0xf
    field_b = (inst_code >>  4) & 0xf
    field_a = (inst_code >>  0) & 0xf
    field_e_needed = field_d == 0xf or \
        (field_c == 0xf and (field_b != 0xf or field_a == 0xf)) or \
        (field_c == 0xe and (field_a == 0xf)) or \
        (field_c  < 0xc and (field_b == 0xf or field_a == 0xf))
    field_e_is_16 = field_d == 0xf or field_a != 0xf or (field_c == 0xf and field_d != 0xf and (field_b & 0xc) == 0)
    if not field_e_needed: return 16
    if field_e_is_16: return 32
    return 48

def get_digits(pattern: str) -> Sequence[int]:
    if pattern in ("0123456789abcdefABCDEF"): return (int(pattern, base=16), )
    if pattern == ".": return (0,1,2,3,4,5,6,7,8,9,0xa,0xb,0xc,0xd,0xe,)
    if pattern == "*": return (0,1,2,3,4,5,6,7,8,9,0xa,0xb,0xc,0xd,0xe,0xf)
    assert False

def is_ext_group(code:int) -> bool:
    # Hard coded known extension groups
    return code in (0xf0ff, 0xf1ff, 0xf4ff, 0xf5ff, 0xf6ff, 0xf7ff, 0xf8ff, 0xf9ff, 0xfaff, 0xfbff) 

class InstCodes(object):
    def __init__(self):
        self.codes = []
        for inst_code in range(0x10000):
            self.codes.append(None)

    def add_code(self, line: str, start: int, sub_code: bool = False):
        inst_len = None
        inst_code = line[start:start+4]
        if not sub_code:
            inst_len = 16
            try:
                if line[start+5:].startswith("..."): inst_len = None
                if line[start+5:].startswith("0x"): inst_len += 16
                if line[start+12:].startswith("0x"): inst_len += 16
            except IndexError:
                pass
        size_conflict_reported = False
        for digit1 in get_digits(inst_code[0]):
            for digit2 in get_digits(inst_code[1]):
                for digit3 in get_digits(inst_code[2]):
                    for digit4 in get_digits(inst_code[3]):
                        inst_code_int = digit1 << 12 | digit2 << 8 | digit3 << 4 | digit4
                        #print(f"0x{inst_code_int:04x}", end=" ")
                        if inst_len is not None and inst_len != get_inst_len(inst_code_int) and not size_conflict_reported:
                            print(f"!!!!!!!!!!!!!!!! SIZE CONFLICT !!!!!!!!!!!!!!!!")
                            print(f"    {line}")
                            print(f"    should be {get_inst_len(inst_code_int)} bits long")
                            size_conflict_reported = True
                        if is_ext_group(inst_code_int):
                            group = inst_codes._add_group(inst_code_int, line)
                            group.add_code(line, start+7, sub_code=True)
                        else:
                            self._add(inst_code_int, line)

    def _add(self, code: int, content: str):
        if self.codes[code] is not None:
            print(f"!!!!!!!!!!!!!!!! CONFLICT !!!!!!!!!!!!!!!!")
            print(f"between:")
            print(f"    {self.codes[code]}")
            print(f"    {content}")
            print(f"    at inst code: 0x{code:04x}")
        self.codes[code] = content

    def _add_group(self, code: int, group: str) -> 'InstCodes':
        if isinstance(self.codes[code], str):
            print(f"!!!!!!!!!!!!!!!! CONFLICT !!!!!!!!!!!!!!!!")
            print(f"between:")
            print(f"    {self.codes[code]}")
            print(f"    extension group {group}")
            print(f"    at inst code: 0x{code:04x}")
        if self.codes[code] is None:
            self.codes[code] = InstCodes()
        return self.codes[code]

inst_codes = InstCodes()

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
            inst_codes.add_code(line, start)
            #print(line)
            pass

print("Code map:")
for inst_code, line in enumerate(inst_codes.codes):
    if line is None:
        print(f"    0x{inst_code:04x} - ")
    elif isinstance(line, InstCodes):
        #for sub_inst_code, sub_line in enumerate(line.codes):
        #    if sub_line is not None:
        #        print(f"    0x{inst_code:04x} 0x{sub_inst_code:04x} - {sub_line}")
        print(f"    0x{inst_code:04x} - MULTIPLE")
    else:
        print(f"    0x{inst_code:04x} - {line}")

# Try to collate groups of unused codes

print("Unused codes:")
#for inst_code, line in enumerate(inst_codes):
#    if line is None:
#        print(f"    0x{inst_code:04x}")

# Try to collate groups of unused codes

from silicon import OrderedSet
unused_codes_16 = OrderedSet(f"{inst_code:04x}" for inst_code, line in enumerate(inst_codes.codes) if line is None and get_inst_len(inst_code) == 16)
unused_codes_32 = OrderedSet(f"{inst_code:04x}" for inst_code, line in enumerate(inst_codes.codes) if line is None and get_inst_len(inst_code) == 32)
unused_codes_48 = OrderedSet(f"{inst_code:04x}" for inst_code, line in enumerate(inst_codes.codes) if line is None and get_inst_len(inst_code) == 48)

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
