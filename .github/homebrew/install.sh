#!/bin/bash
# Install the JADE Homebrew formulae (.github/homebrew/Formula) into the local tap jade/hep.
# Uses the bottles on ghcr.io when they exist for the current recipe and macOS,
# otherwise Homebrew builds from source.
# Usage: bash .github/homebrew/install.sh [formula ...]      (default: cernlib hepmc3)
#   JADE_BOTTLES_OWNER   GitHub owner of the bottles (default: $GITHUB_REPOSITORY_OWNER or andriish)
#   JADE_REINSTALL=1     reinstall formulae that are already installed
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
owner=$(echo "${JADE_BOTTLES_OWNER:-${GITHUB_REPOSITORY_OWNER:-andriish}}" | tr '[:upper:]' '[:lower:]')
root_url="https://ghcr.io/v2/${owner}/jade"
tap=jade/hep
if [ $# -gt 0 ]; then formulae=("$@"); else formulae=(cernlib hepmc3); fi

tapdir="$(brew --repository)/Library/Taps/jade/homebrew-hep"
[ -d "$tapdir" ] || brew tap-new --no-git "$tap"
mkdir -p "$tapdir/Formula"

for f in "${formulae[@]}"; do
  src="$here/Formula/$f.rb"
  dst="$tapdir/Formula/$f.rb"
  rebuild=$(python3 -I "$here/bottles.py" rebuild "$src")
  python3 -I "$here/bottles.py" prepare "$src" "$dst" "$rebuild"
  pkgver=$(brew info --json=v2 "$tap/$f" | python3 -I "$here/bottles.py" pkgver)
  if python3 -I "$here/bottles.py" fetch "$root_url" "$f" "$pkgver" "$rebuild" "$src" "$dst"; then
    echo "$f: bottles $pkgver-$rebuild found on ghcr.io"
  else
    echo "$f: no bottles $pkgver-$rebuild on ghcr.io, building from source"
  fi
  if brew list --formula "$tap/$f" >/dev/null 2>&1; then
    if [ "${JADE_REINSTALL:-0}" = "1" ]; then brew reinstall "$tap/$f"; else echo "$f: already installed"; fi
  else
    brew install "$tap/$f"
  fi
done
