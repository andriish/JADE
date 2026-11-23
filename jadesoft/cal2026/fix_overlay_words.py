#!/usr/bin/env python3
"""Second step of the conversion of the JADE calibration files for a little-endian host.

conv (util2016/ccal/conv.f) packs the half-word constants of the ASCII files two per word, the
first one in the low half on a little-endian host. That is the memory order the program needs for
the half-word arrays (INTEGER*2), so these data are used as written.

The muon calibration (bank MUCA, common block layout in src2016/jademus/cmucalib.for) has
INTEGER*4 variables that are EQUIVALENCEd to a pair of half-words:
    NFRAMS  = HMFFIX(1..2)  MUCAL word   20
    NCHAMS  = HMCFIX(1..2)  MUCAL word  390
    IFCIND  = HFILDA(61..72) MUCAL words 4155-4160
On a big-endian machine the first half-word of the pair is the high half of the integer, on a
little-endian machine it is the low half, so the two halves of these words have to be exchanged.
All other data stay as written by conv.

usage: fix_overlay_words.py in.b out.b      (files in the Fortran unformatted, big-endian format)
"""
import struct, sys

MUCA = b"MUCA"[::-1]   # the name word written by conv on a little-endian host reads reversed
NHEADW = 10            # header words of a record: length, name, ..., IFADDR=word 8
SWAP_WORDS = [20, 390] + list(range(4155, 4161))   # words of the MUCAL image (1-based)

def main(src, dst):
    d = open(src, "rb").read()
    out = bytearray()
    pos = 0
    nrec = nfix = 0
    while pos < len(d):
        (n,) = struct.unpack(">i", d[pos:pos + 4])
        body = bytearray(d[pos + 4:pos + 4 + n])
        (n2,) = struct.unpack(">i", d[pos + 4 + n:pos + 8 + n])
        assert n == n2, "bad record markers"
        if n > 4 * NHEADW and bytes(body[4:8]) == MUCA:
            ifaddr = struct.unpack(">i", body[28:32])[0]    # header word 8
            if ifaddr > 0:                  # a block of consecutive constants (not an update list)
                first = (ifaddr - 1) // 2   # 0-based MUCAL word of the first data word
                ndata = n // 4 - NHEADW
                for w in SWAP_WORDS:
                    i = w - 1 - first
                    if 0 <= i < ndata:
                        o = 4 * (NHEADW + i)
                        body[o:o + 4] = body[o + 2:o + 4] + body[o:o + 2]
                        nfix += 1
        out += struct.pack(">i", n) + body + struct.pack(">i", n)
        pos += 8 + n
        nrec += 1
    open(dst, "wb").write(out)
    print("%s: %d records, %d words exchanged" % (dst, nrec, nfix))

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
