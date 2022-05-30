from typing import *
from silicon import *
from brew_types import *

@module(1)
def field_d(inst_word):
    return inst_word[15:12]

@module(1)
def field_c(inst_word):
    return inst_word[11:8]

@module(1)
def field_b(inst_word):
    return inst_word[7:4]

@module(1)
def field_a(inst_word):
    return inst_word[3:0]

