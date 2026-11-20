#!/usr/bin/env python3
"""Check that JADE display hardcopies are coloured.

usage: check_colours.py file...   (JADE.nnn.png and/or JADE.nnn.ps)

PNG : counts the distinct saturated (non-grey) colours; needs Pillow, or skips.
PS  : counts the distinct 'r g b c' colour settings written by HIGZ.
Exit code 0 if at least MINCOL (2) distinct non-grey colours are found.
"""
import os
import re
import sys

MINCOL = 2
found = set()
for f in sys.argv[1:]:
    if not os.path.isfile(f):
        continue
    if f.endswith('.png'):
        try:
            from PIL import Image
        except ImportError:
            print('Pillow not available, PNG colours not checked')
            sys.exit(0)
        im = Image.open(f).convert('RGB')
        for n, (r, g, b) in im.getcolors(maxcolors=1 << 24):
            if max(r, g, b) - min(r, g, b) > 60 and n > 3:
                found.add((r // 64, g // 64, b // 64))
    else:
        txt = open(f, errors='replace').read()
        for r, g, b in re.findall(r'([0-9.]+) ([0-9.]+) ([0-9.]+) c(?: |$)', txt, re.M):
            r, g, b = float(r), float(g), float(b)
            if max(r, g, b) - min(r, g, b) > 0.2:
                found.add((int(r * 3), int(g * 3), int(b * 3)))
print('distinct non-grey colours: %d %s' % (len(found), sorted(found)))
sys.exit(0 if len(found) >= MINCOL else 1)
