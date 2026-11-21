# Homebrew formulae and bottles for JADE on macOS

`Formula/` holds the Homebrew recipes JADE needs that are not (or not in the right
version) in homebrew-core:

| Formula | Version | Origin |
|---|---|---|
| `cernlib` | 2026.11.01.1 | [davidchall/homebrew-hep](https://github.com/davidchall/homebrew-hep), plus `CMAKE_POLICY_VERSION_MINIMUM=3.5` for CMake 4 |
| `hepmc3` | 3.3.1 | davidchall/homebrew-hep, updated to the release from the [HepMC3 project page](https://hepmc.web.cern.ch/hepmc/); no Python bindings, no ROOT I/O |

The recipes have no `bottle do` block. Bottles are built by
`.github/workflows/homebrew-bottles.yml` on the newest GitHub macOS image and pushed to
GitHub Packages as `ghcr.io/<owner>/jade/<formula>:<pkg_version>-<rebuild>`.
The rebuild number is computed from the SHA-256 of the recipe file
(`bottles.py rebuild`), so:

- changing a recipe (and pushing it) builds and publishes new bottles;
- an unchanged recipe is detected on ghcr.io and the workflow does nothing;
- the workflow can also be started by hand (`workflow_dispatch`).

## Installing

```sh
bash .github/homebrew/install.sh            # cernlib and hepmc3
bash .github/homebrew/install.sh cernlib    # only one
```

The script creates a local tap `jade/hep` (no git), copies the recipes there, reads the
bottle digests for the current recipe from ghcr.io and writes the `bottle do` block, then
runs `brew install jade/hep/<formula>`. Without a matching bottle (another macOS version,
recipe not yet published) Homebrew builds from source. `JADE_REINSTALL=1` reinstalls
formulae that are already installed; `JADE_BOTTLES_OWNER` selects another GitHub owner.

Notes:

- The packages must be public on GitHub Packages for anonymous download (package
  settings, "Change visibility"), the first time they are pushed.
- A formula with the same name from another tap (e.g. `davidchall/hep/hepmc3`) must be
  uninstalled first.
- Bottles are poured with `cellar: :any`, i.e. they expect the standard Homebrew prefix
  (`/opt/homebrew` on Apple Silicon).
