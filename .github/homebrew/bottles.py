#!/usr/bin/env python3
"""Helpers for the JADE Homebrew bottles (see .github/homebrew/README.md).

The bottles live on GitHub Packages (ghcr.io) under <root_url>/<formula>,
tagged <pkg_version>-<rebuild>.  The rebuild number is derived from the
SHA-256 of the formula file in .github/homebrew/Formula, so every change of a
recipe gives a new tag and therefore new bottles.

Subcommands:
  rebuild FORMULA.rb                 print the rebuild number of a recipe
  pkgver                             read `brew info --json=v2` on stdin, print pkg_version
  prepare SRC.rb DST.rb REBUILD      copy recipe with a bottle block that only sets the rebuild
  exists ROOT_URL NAME PKGVER REBUILD   exit 0 if the bottles are published
  fetch ROOT_URL NAME PKGVER REBUILD SRC.rb DST.rb
                                     copy recipe with the full bottle block read from ghcr.io;
                                     exit 3 if no bottles are published
"""
import hashlib
import json
import re
import sys
import urllib.error
import urllib.request

INDEX_TYPES = ", ".join([
    "application/vnd.oci.image.index.v1+json",
    "application/vnd.docker.distribution.manifest.list.v2+json",
])


def rebuild_number(path):
    with open(path, "rb") as f:
        digest = hashlib.sha256(f.read()).hexdigest()
    # 6 hex digits: unique enough, and a valid OCI tag / bottle file name.
    return int(digest[:6], 16) + 1


def insert_bottle_block(src, dst, block_lines):
    with open(src) as f:
        lines = f.readlines()
    for i, line in enumerate(lines):
        if re.match(r"^  license\s", line):
            break
    else:
        sys.exit(f"{src}: no 'license' line to put the bottle block after")
    block = ["\n", "  bottle do\n"] + [f"    {l}\n" for l in block_lines] + ["  end\n"]
    with open(dst, "w") as f:
        f.writelines(lines[: i + 1] + block + lines[i + 1:])


def image_index(root_url, name, tag):
    url = f"{root_url}/{name}/manifests/{tag}"
    # Anonymous access to public packages, as Homebrew does it.
    req = urllib.request.Request(url, headers={"Accept": INDEX_TYPES,
                                               "Authorization": "Bearer QQ=="})
    try:
        with urllib.request.urlopen(req, timeout=60) as r:
            return json.load(r)
    except urllib.error.HTTPError as e:
        if e.code in (401, 403, 404):
            return None
        raise


def main(argv):
    if len(argv) < 2:
        sys.exit(__doc__)
    cmd, args = argv[1], argv[2:]
    if cmd == "rebuild" and len(args) == 1:
        print(rebuild_number(args[0]))
    elif cmd == "pkgver" and not args:
        formula = json.load(sys.stdin)["formulae"][0]
        version = formula["versions"]["stable"]
        revision = int(formula.get("revision") or 0)
        print(f"{version}_{revision}" if revision else version)
    elif cmd == "prepare" and len(args) == 3:
        src, dst, rebuild = args
        insert_bottle_block(src, dst, [f"rebuild {int(rebuild)}"])
    elif cmd == "exists" and len(args) == 4:
        root_url, name, pkgver, rebuild = args
        sys.exit(0 if image_index(root_url, name, f"{pkgver}-{rebuild}") else 1)
    elif cmd == "fetch" and len(args) == 6:
        root_url, name, pkgver, rebuild, src, dst = args
        index = image_index(root_url, name, f"{pkgver}-{rebuild}")
        if not index:
            sys.exit(3)
        lines = [f'root_url "{root_url}"', f"rebuild {int(rebuild)}"]
        prefix, suffix = f"{pkgver}.", f".{rebuild}"
        for m in index.get("manifests", []):
            ann = m.get("annotations", {})
            ref = ann.get("org.opencontainers.image.ref.name", "")
            digest = ann.get("sh.brew.bottle.digest")
            if not (digest and ref.startswith(prefix) and ref.endswith(suffix)):
                continue
            tag = ref[len(prefix):-len(suffix)]
            # The cellar type is not stored on ghcr.io; ":any" pours into the
            # standard Homebrew prefix (/opt/homebrew, /usr/local).
            lines.append(f'sha256 cellar: :any, {tag}: "{digest}"')
        if len(lines) == 2:
            sys.exit(3)
        insert_bottle_block(src, dst, lines)
        print("\n".join(lines[2:]))
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main(sys.argv)
