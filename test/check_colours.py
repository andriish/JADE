#!/usr/bin/env python3
"""Check that JADE display hardcopies are coloured.

usage: check_colours.py [--grey] file...   (JADE.nnn.png and/or JADE.nnn.ps)
  --grey : also require the thin grey detector boundary lines (JADE colour 0.4 0.4 0.4)
  --dedx : also require the small yellow dE/dx plot in the bottom-left corner (PNG screen pictures only)

PNG : counts the distinct saturated (non-grey) colours; needs Pillow, or skips.
PS  : counts the distinct 'r g b c' colour settings written by HIGZ.
Exit code 0 if at least MINCOL (2) distinct non-grey colours are found and the
orange of the lead glass numbers (ECAL, drawn with the HEMSYM stroke font) is present.
"""
import os
import re
import sys

MINCOL = 2
found = set()
orange = False
grey = 0
need_grey = '--grey' in sys.argv
need_dedx = '--dedx' in sys.argv
dedx_ok = False
args = [a for a in sys.argv[1:] if a not in ('--grey', '--dedx')]
for f in args:
    if not os.path.isfile(f):
        continue
    if f.endswith('.png'):
        try:
            from PIL import Image
        except ImportError:
            print('Pillow not available, PNG colours not checked')
            sys.exit(0)
        im = Image.open(f).convert('RGB')
        if f.endswith('.screen.png'):
            w, h = im.size
            box = im.crop((int(w * 0.10), int(h * 0.86), int(w * 0.32), int(h * 0.98)))
            ny = sum(n for n, c in box.getcolors(maxcolors=1 << 24) if c == (255, 255, 0))
            if ny > 300:
                dedx_ok = True
        for n, (r, g, b) in im.getcolors(maxcolors=1 << 24):
            if max(r, g, b) - min(r, g, b) > 60 and n > 3:
                found.add((r // 64, g // 64, b // 64))
            if r > 200 and 100 < g < 200 and b < 60 and n > 20:
                orange = True
            if (abs(r - 102) < 12 and abs(g - 102) < 12 and abs(b - 102) < 12) or (abs(r - 204) < 12 and abs(g - 204) < 12 and abs(b - 204) < 12):
                grey += n
    else:
        txt = open(f, errors='replace').read()
        for r, g, b in re.findall(r'([0-9.]+) ([0-9.]+) ([0-9.]+) c(?: |$)', txt, re.M):
            r, g, b = float(r), float(g), float(b)
            if max(r, g, b) - min(r, g, b) > 0.2:
                found.add((int(r * 3), int(g * 3), int(b * 3)))
            if r > 0.9 and 0.4 < g < 0.8 and b < 0.1:
                orange = True
            if abs(r - 0.4) < 0.05 and abs(g - 0.4) < 0.05 and abs(b - 0.4) < 0.05:
                grey += 1000
print('distinct non-grey colours: %d %s, orange (lead glass numbers): %s, grey boundary pixels: %d' % (len(found), sorted(found), orange, grey))
if need_dedx:
    print('small dE/dx plot (bottom left): %s' % dedx_ok)
ok = len(found) >= MINCOL and orange and (grey >= 2000 or not need_grey) and (dedx_ok or not need_dedx)
sys.exit(0 if ok else 1)
