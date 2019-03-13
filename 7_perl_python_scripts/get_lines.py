#!/usr/bin/env python3
import sys

#-- set the minimum number below
n = 20
# don't change anything below
f = sys.argv[1]; out = sys.argv[2]; mark1 = ""; lines = []

def write_out(lines):
    if len(lines) >= n:
        with open(out, "a+") as wrt:
            for line in lines:
                wrt.write(line)

with open(f) as read:
    for l in read:
        mark2 = l.split()[0]
        if mark2 == mark1:
            lines.append(l)
        else:
            write_out(lines)
            lines = [l]
        mark1 = mark2
# add the last set of lines
write_out(lines)
