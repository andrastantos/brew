#!/usr/bin/env python3
"""Compare two run-opt-tests.sh runs.

Usage: brew/compare-opt-tests.py OLD NEW [--all]

OLD and NEW are run names (directories under test-results/) or paths to
their opt/ directories.  For each test and optimization level, prints the
executed instructions, taken branches, code size and Espresso cycles (runs
since 2026-11-24; '-' for older ones) of both runs with the relative change;
only rows that changed are shown unless --all is given.
The totals per optimization level leave out empty (startup only) and
dhrystone.  A status other than ok in NEW is flagged.
"""

import os
import sys

TOP = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def load(arg):
    path = arg
    if not os.path.isdir(path):
        path = os.path.join(TOP, "test-results", arg)
    if os.path.isdir(os.path.join(path, "opt")):
        path = os.path.join(path, "opt")
    rows = {}
    with open(os.path.join(path, "results.txt")) as f:
        for line in f:
            if line.startswith("#") or not line.strip():
                continue
            f = line.split()
            t, opt, status, insns, taken, untaken, text = f[:7]
            rows[(t, opt)] = dict(status=status, insns=int(insns),
                                  taken=int(taken), untaken=int(untaken),
                                  text=int(text),
                                  cycles=int(f[7]) if len(f) > 7 else None)
    return rows


def pct(old, new):
    if old == new:
        return ""
    if old == 0:
        return "new"
    return "%+.2f%%" % (100.0 * (new - old) / old)


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    show_all = "--all" in sys.argv[1:]
    if len(args) != 2:
        sys.exit(__doc__)
    old, new = load(args[0]), load(args[1])
    fmt = "%-16s %-3s %10s %10s %8s  %8s %8s %8s  %6s %6s %8s  %10s %10s %8s  %s"
    print(fmt % ("test", "opt", "insns", "", "", "taken", "", "", "text", "", "",
                 "cycles", "", "", ""))
    have_cycles = all(r["cycles"] is not None for r in list(old.values()) + list(new.values()))
    fields = ("insns", "taken", "text", "cycles") if have_cycles else ("insns", "taken", "text")

    def cyc(r):
        return r["cycles"] if r["cycles"] is not None else "-"

    def cpct(o, n):
        return pct(o["cycles"], n["cycles"]) if have_cycles else ""
    totals = {}
    for key in sorted(set(old) & set(new), key=lambda k: (k[1], k[0])):
        o, n = old[key], new[key]
        t, opt = key
        if t not in ("empty", "dhrystone"):
            tot = totals.setdefault(opt, [0] * 8)
            for i, f in enumerate(fields):
                tot[2 * i] += o[f]
                tot[2 * i + 1] += n[f]
        flag = "" if n["status"] in ("ok", "-") else n["status"]
        changed = any(o[f] != n[f] for f in fields)
        if changed or show_all or flag:
            print(fmt % (t, opt, o["insns"], n["insns"], pct(o["insns"], n["insns"]),
                         o["taken"], n["taken"], pct(o["taken"], n["taken"]),
                         o["text"], n["text"], pct(o["text"], n["text"]),
                         cyc(o), cyc(n), cpct(o, n), flag))
    for opt, (oi, ni, ot, nt, os_, ns, oc, nc) in sorted(totals.items()):
        print(fmt % ("TOTAL", opt, oi, ni, pct(oi, ni), ot, nt, pct(ot, nt),
                     os_, ns, pct(os_, ns),
                     oc if have_cycles else "-", nc if have_cycles else "-",
                     pct(oc, nc) if have_cycles else "", ""))
    missing = sorted(set(old) ^ set(new))
    if missing:
        print("only in one run:", " ".join("%s-%s" % k for k in missing))


if __name__ == "__main__":
    main()
