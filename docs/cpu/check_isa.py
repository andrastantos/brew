###!/usr/bin/python
from typing import Sequence, Union, Generator, Tuple

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

def list_inst_codes(inst_code: str) -> 'Generator':
    if inst_code.startswith("0x"): inst_code = inst_code[2:]
    for digit1 in get_digits(inst_code[0]):
        for digit2 in get_digits(inst_code[1]):
            for digit3 in get_digits(inst_code[2]):
                for digit4 in get_digits(inst_code[3]):
                    inst_code_int = digit1 << 12 | digit2 << 8 | digit3 << 4 | digit4
                    yield inst_code_int

from dataclasses import dataclass

@dataclass
class InstInfo(object):
    inst_code: str
    asm: str
    operation: str
    detail: bool = False

    def __str__(self) -> str:
        return f"{self.inst_code}    {self.asm}   :   {self.operation}"

def split_inst_words(inst_code: str) -> Tuple[Sequence[str], int]:
    inst_len = None
    inst_words = inst_code.split()
    inst_len = len(inst_words) * 16
    if len(inst_words) > 1 and inst_words[1] == "...": inst_len = None
    return inst_words, inst_len

class InstCodes(object):
    def __init__(self):
        self.codes = []
        for inst_code in range(0x10000):
            self.codes.append(None)

    def add_code(self, inst_code: str, asm: str, operation: str, start: int, sub_code: bool = False):
        inst_words, inst_len = split_inst_words(inst_code)
        size_conflict_reported = False
        for inst_code_int in list_inst_codes(inst_code):
            #print(f"0x{inst_code_int:04x}", end=" ")
            if inst_len is not None and inst_len != get_inst_len(inst_code_int) and not size_conflict_reported and not sub_code:
                print(f"!!!!!!!!!!!!!!!! SIZE CONFLICT !!!!!!!!!!!!!!!!")
                print(f"    {inst_code} {asm}")
                print(f"    should be {get_inst_len(inst_code_int)} bits long")
                size_conflict_reported = True
            if is_ext_group(inst_code_int):
                group = inst_codes._add_group(inst_code_int, asm)
                group.add_code(" ".join(inst_words[1:]), asm, operation, start+7, sub_code=True)
            else:
                self._add(inst_code_int, inst_code, asm, operation)

    def _add(self, code: int, inst_code: str, asm: str, operation: str):
        content = InstInfo(inst_code, asm, operation)
        if self.codes[code] is not None:
            print(f"!!!!!!!!!!!!!!!! CONFLICT !!!!!!!!!!!!!!!!")
            print(f"between:")
            print(f"    {self.codes[code]}")
            print(f"    {content}")
            print(f"    at inst code: 0x{code:04x}")
        self.codes[code] = content

    def _add_group(self, code: int, group: str) -> 'InstCodes':
        if isinstance(self.codes[code], InstInfo):
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
    sections = None
    while file:
        line = file.readline()
        if line == "":
            break
        if line.endswith("\n"): line = line[:-1]
        if "see below" in line: continue
        if line.startswith("====="):
            sections = []
            pc = None
            for idx, c in enumerate(line):
                if c == "=" and pc == " ":
                    sections.append(idx)
                if c not in "= ":
                    sections = None
                    break
                pc = c
            if sections is not None:
                sections.append(idx)
            pass
        if line.startswith("0x") and len(sections) >= 3:
            inst_code = line[0:sections[0]].strip()
            asm = line[sections[0]:sections[1]].strip()
            operation = line[sections[1]:].strip()

            inst_codes.add_code(inst_code, asm, operation, 0)
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

# Look through detailed instruction specs for inconsistencies
from glob import glob

pass

for detail_file_name in glob("isa_detail*.rst"):
    def is_underscore(s: str) -> bool:
        return all(s == "-" for s in s) and len(s) > 0

    inst_name = None
    with open(detail_file_name, "rt") as file:
        line = None
        line_num = 0
        while file:
            line_num += 1
            prev_line = line
            line = file.readline()
            if line == "":
                break
            if line.endswith("\n"): line = line[:-1]
            if is_underscore(line):
                # We've found a new instruction
                inst_name = prev_line
            sline = line.strip()
            if sline.startswith("*Instruction code*:"):
                inst_code_str = sline[len("*Instruction code*:")+1:].strip()
                if inst_name is None:
                    print("========== CONSISTENCY ERROR ===============")
                    print(f"    {detail_file_name}:{line_num} - instruction code: {inst_code_str} occurs without instruction name")
                else:
                    inst_words, inst_len = split_inst_words(inst_code_str)
                    size_conflict_reported = False
                    for inst_code_int in list_inst_codes(inst_code_str):
                        if inst_len is not None and inst_len != get_inst_len(inst_code_int) and not size_conflict_reported:
                            print(f"========== SIZE CONFLICT ==========")
                            print(f"    {detail_file_name}:{line_num} - instruction code: {inst_code_str} should be {get_inst_len(inst_code_int)} bits long")
                            size_conflict_reported = True
                        summary = inst_codes.codes[inst_code_int]
                        if summary is None:
                            print("========== CONSISTENCY ERROR ===============")
                            print(f"    {detail_file_name}:{line_num} - instruction detail:")
                            print(f"         '{inst_name}'")
                            print(f"    for code 0x{inst_code_int:04x} doesn't have summary")
                        if isinstance(summary, InstInfo):
                            summary.detail = True
                            if inst_name not in summary.asm: # This could be more stringent by parsing the summary by (probably) understanding the table structure of RTS.
                                if "Type override" in summary.operation and inst_name.startswith("Type override"):
                                    continue
                                if inst_name.startswith("if $rA[C]") and "if $rA[" in summary.asm:
                                    continue
                                if inst_name.startswith("if $rB[C]") and "if $rB[" in summary.asm:
                                    continue
                                print("========== CONSISTENCY ERROR ===============")
                                print(f"    {detail_file_name}:{line_num} - instruction detail:")
                                print(f"         '{inst_name}'")
                                print(f"    for code 0x{inst_code_int:04x} doesn't match summary")
                                print(f"         '{summary}'")

print("Instructions with no detail:")
for inst_code, line in enumerate(inst_codes.codes):
    if line is None:
        continue
    elif isinstance(line, InstCodes):
        #for sub_inst_code, sub_line in enumerate(line.codes):
        #    if sub_line is not None:
        #        print(f"    0x{inst_code:04x} 0x{sub_inst_code:04x} - {sub_line}")
        #print(f"    0x{inst_code:04x} - MULTIPLE")
        pass
    else:
        if not line.detail:
            print(f"    0x{inst_code:04x} - {line}")
