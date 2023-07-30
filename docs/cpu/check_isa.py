###!/usr/bin/python
from typing import Sequence, Union, Generator, Tuple, List, Optional
from pathlib import Path

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


import re

def canonical_asm(asm: str) -> str:
    asm = re.sub(r"\$rA\[([0-9])+\]", "$rA[C]", asm)
    asm = re.sub(r"\$rB\[([0-9])+\]", "$rB[C]", asm)
    asm = " ".join(x for x in asm.split(sep=" ") if len(x) > 0)
    #asm = asm.replace("&lt;", "<")
    #asm = asm.replace("&gt;", ">")
    return asm

def escape_link_text(link_text: str) -> str:
    #link_text = link_text.replace("<", "&lt;")
    #link_text = link_text.replace(">", "&gt;")
    return link_text

inst_to_anchor_map = {}
def name_to_anchor(inst_name: str) -> str:
    c_inst_name = canonical_asm(inst_name)
    if c_inst_name in inst_to_anchor_map:
        return inst_to_anchor_map[c_inst_name]
    inst_name = inst_name.replace('<-', 'eq')
    inst_name = inst_name.replace('-$', 'minus_')
    inst_name = inst_name.replace('$', '')
    inst_name = inst_name.replace('[.]', '_bit')
    inst_name = inst_name.replace('[', '_')
    inst_name = inst_name.replace(']', '')
    inst_name = inst_name.replace('>>>', 'asr')
    inst_name = inst_name.replace('>>', 'lsr')
    inst_name = inst_name.replace('<<', 'lsl')
    inst_name = inst_name.replace('&', 'and')
    inst_name = inst_name.replace('|', 'or')
    inst_name = inst_name.replace('^', 'xor')
    inst_name = inst_name.replace('+', 'plus')
    inst_name = inst_name.replace('-', 'minus')
    inst_name = inst_name.replace('*', 'times')
    inst_name = inst_name.replace('~', 'not')
    inst_name = inst_name.replace('<=', 'le')
    inst_name = inst_name.replace('>=', 'ge')
    inst_name = inst_name.replace('<', 'lt')
    inst_name = inst_name.replace('>', 'gt')
    inst_name = inst_name.replace('==', 'eq')
    inst_name = inst_name.replace('!=', 'ne')
    inst_name = inst_name.replace(' ', '_')
    if inst_name[-1] == "_": inst_name = inst_name[:-1] + "\\_"
    inst_name = inst_name.lower()
    inst_to_anchor_map[c_inst_name] = inst_name
    return inst_name

def extract_inst_code(asm: str) -> Tuple[str, Optional[str]]:
    def _extract_inst_code(asm):
        if asm.startswith(":ref:"):
            # we already have a reference. Ensure it's what we would insert and move on...
            target_start = asm.rfind("<")
            if target_start != -1:
                target = asm[target_start+1:]
                if target[-2:] != ">`":
                    print("=========== CONSISTENCY ERROR =============")
                    print(f"      can't parse reference target in: '{asm}'")
                    return asm, None
                target = target[:-2]
                asm_start = asm.find("`")
                if asm_start != -1:
                    true_asm = asm[asm_start+1:target_start].strip()
                    return true_asm, target
                else:
                    print("=========== CONSISTENCY ERROR =============")
                    print(f"      can't parse reference target in: '{asm}'")
                    return asm, None
            else:
                print("=========== CONSISTENCY ERROR =============")
                print(f"      can't parse reference target in: '{asm}'")
                return asm, None
        else:
            return asm, None
    asm, target = _extract_inst_code(asm)
    return asm, target

def is_summary_line(line: str) -> bool:
    return line.startswith("0x") or line.startswith(":ref:`0x")

isa_summary = []

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
        if is_summary_line(line) and len(sections) >= 3:
            inst_code, _ = extract_inst_code(line[0:sections[0]].strip())
            asm = canonical_asm(line[sections[0]:sections[1]].strip())
            operation = line[sections[1]:].strip()

            inst_codes.add_code(inst_code, asm, operation, 0)
            isa_summary.append((inst_code, asm, operation))
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

last_anchor = None
for detail_file_name in glob("isa_detail*.rst"):
    def is_underscore(s: str) -> bool:
        return all(s == "-" for s in s) and len(s) > 0

    if detail_file_name == "isa_detail_new.rst":
        continue
    inst_name = None
    inst_start_line = None
    out_file = open(Path(detail_file_name).with_suffix(".out_rst"), "wt")
    with open(detail_file_name, "rt") as file:
        line : str = None
        line_num = 0
        while file:
            line_num += 1
            prev_line = line
            line = file.readline()
            if line == "":
                out_file.write(f"{prev_line}\n")
                out_file.close()
                break
            if line.endswith("\n"): line = line[:-1]
            if line.startswith(".. _") and line.endswith(":"):
                last_anchor = line[4:-1]
            if is_underscore(line):
                # We've found a new instruction
                inst_name = prev_line
                inst_start_line = line_num - 1
                if last_anchor is None:
                    out_file.write(f".. _{name_to_anchor(inst_name)}:\n\n")
                else:
                    inst_to_anchor_map[canonical_asm(inst_name)] = last_anchor
                last_anchor = None
            if prev_line is not None: out_file.write(f"{prev_line}\n")
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
                            if canonical_asm(inst_name) != canonical_asm(summary.asm):
                                if "Type override" in summary.operation and inst_name.startswith("Type override"):
                                    continue
                                print("========== CONSISTENCY ERROR ===============")
                                print(f"    {detail_file_name}:{line_num} - instruction detail:")
                                print(f"         '{inst_name}'")
                                print(f"    for code 0x{inst_code_int:04x} doesn't match summary")
                                print(f"         '{summary}'")

print("Instructions with no detail:")
reported_missing = OrderedSet()

with open("isa_detail_new.rst", "wt") as new_inst_file:
    line: InstInfo
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
                if line.inst_code not in reported_missing:
                    new_inst_file.write(f"{line.asm}\n");
                    new_inst_file.write(f"{'-'*(len(line.asm)+1)}\n");
                    new_inst_file.write(f"\n");
                    new_inst_file.write(f"*Instruction code*: {line.inst_code}\n");
                    new_inst_file.write(f"\n");
                    new_inst_file.write(f"*Exceptions*: TBD\n");
                    new_inst_file.write(f"\n");
                    new_inst_file.write(f"*Type variants*: TBD\n");
                    new_inst_file.write(f"\n");
                    new_inst_file.write(f"Description\n");
                    new_inst_file.write(f"~~~~~~~~~~~\n");
                    new_inst_file.write(f"\n");
                    new_inst_file.write(f"{line.operation}\n");
                    new_inst_file.write(f"\n");
                    new_inst_file.write(f"\n");
                reported_missing.add(line.inst_code)

# re-write summary with links to details

from enum import Enum

class TableStates(Enum):
    outside = 0
    header = 1
    body = 2

out_file = open("isa.out_rst", "wt")
table_layout = ""
table_states = TableStates.outside
table_headers = None
table_content = None

def split_table_row(line: str) -> List[str]:
    fields = []
    start = 0
    for end in sections[:-1] + [len(line)]:
        header = line[start:end].strip()
        fields.append(header)
        start = end
    return fields


with open("isa.rst", "rt") as file:
    sections = None
    line_idx = 0
    while file:
        line = file.readline()
        line_idx += 1
        if line == "":
            break
        if line.endswith("\n"): line = line[:-1]
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
                sections.append(idx+2)
            if len(sections) >= 3:
                if table_states == TableStates.outside:
                    table_states = TableStates.header
                    table_layout = line
                    table_headers = None
                    table_content = None
                elif table_states == TableStates.header:
                    if table_layout != line:
                        print(f"======= Table parsing error at line {line_idx}")
                    table_states = TableStates.body
                elif table_states == TableStates.body:
                    if table_layout != line:
                        print(f"======= Table parsing error at line {line_idx}")
                    table_states = TableStates.outside
                    if table_content is None or table_headers is None:
                        print(f"======= Table parsing error at line {line_idx}: no header or content")
                    # Re-create the table with potentially adjusted field sizes
                    field_sizes = []
                    start = 0
                    for end in sections:
                        field_sizes.append(end-start-1)
                        start = end
                    for idx, field in enumerate(table_headers):
                        field_sizes[idx] = max(field_sizes[idx], len(field))
                    for row in table_content:
                        for idx, field in enumerate(row):
                            field_sizes[idx] = max(field_sizes[idx], len(field))
                    table_def = " ".join("="*f for f in field_sizes)
                    out_file.write(f"{table_def}\n")
                    out_line = ""
                    for idx, field in enumerate(table_headers):
                        out_line += f"{field:{field_sizes[idx]}} "
                    out_file.write(f"{out_line.rstrip()}\n")
                    out_file.write(f"{table_def}\n")
                    for row in table_content:
                        out_line = ""
                        for idx, field in enumerate(row):
                            out_line += f"{field:{field_sizes[idx]}} "
                        out_file.write(f"{out_line.rstrip()}\n")
                    out_file.write(f"{table_def}\n")
            else:
                out_file.write(f"{line}\n")
        else:
            if table_states == TableStates.header:
                table_headers = split_table_row(line)
            elif table_states == TableStates.body:
                fields = split_table_row(line)
                if is_summary_line(fields[0]) and len(sections) >= 3:
                    _, target = extract_inst_code(fields[0])
                    asm = fields[1]
                    if target is None:
                        if "see below" not in asm:
                            try:
                                link = inst_to_anchor_map[canonical_asm(asm)]
                                fields[0] = f":ref:`{escape_link_text(fields[0])}<{link}>`"
                            except KeyError:
                                print("=========== CONSISTENCY ERROR =============")
                                print(f"      asm: '{asm}' for inst code '{fields[0]}' is not in anchor map")
                    else:
                        try:
                            link = inst_to_anchor_map[canonical_asm(asm)]
                            if link != target:
                                print("=========== CONSISTENCY ERROR =============")
                                print(f"      asm: '{asm}' points to '{target}' instead if {link}")
                        except KeyError:
                            print("=========== CONSISTENCY ERROR =============")
                            print(f"      asm: '{asm}' points to '{target}', yet somehow it's missing from anchor map")
                if table_content is None: table_content = []
                table_content.append(fields.copy())
            else:
                out_file.write(f"{line}\n")
out_file.close()

#def sort_opcode(item1, item2):
#    # opcodes are strings in the form of 0xXXXX 0xXXXX 0xXXXX ...
#    # What we want is to sort
#    if fitness(item1) < fitness(item2):
#        return -1
#    elif fitness(item1) > fitness(item2):
#        return 1
#    else:
#        return 0

# Writing out condensed ISA table
with open("isa_template.rst", "wt") as out_file:
    inst_header = "Instruction code"
    asm_header= "Assembly"
    operation_header = "Operation"
    inst_code_len = len(inst_header)
    asm_len = len(asm_header)
    operation_len = len(operation_header)

    for inst_code, asm, operation in isa_summary:
        inst_code_len = max(inst_code_len, len(inst_code))
        asm_len = max(asm_len, len(asm))
        operation_len = max(operation_len, len(operation))


    lines = f"{'='*inst_code_len}   {'='*asm_len}   ================\n"
    out_file.write(lines)
    out_file.write(f"{inst_header:{inst_code_len}}   {asm_header:{asm_len}}   Implemented\n")
    out_file.write(lines)
    #for inst_code, asm, operation in sorted(isa_summary):
    for inst_code, asm, operation in isa_summary:
        out_file.write(f"{inst_code:{inst_code_len}}   {asm:{asm_len}}\n")
    out_file.write(lines)

