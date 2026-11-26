# CERNLIB RPMs for Fedora 45 and CentOS Stream 9 and 10

`cernlib.spec`, `patch-cernlib-0.txt` and `cernlib-s390x-bigendian.patch` are the recipe of the package `cernlib`
2026.11.01.1 (release 4) from https://github.com/andriish/HEPrpms, branch `JADE`, directory
`cernlib/2026.11.01.1` (commit `d12f2da`). The sources are downloaded from CERN when the package is built.

`build.sh <arch> <output directory>` builds the packages inside the default image of the distribution (the build
dependencies come from dnf, on CentOS with EPEL and CRB; the sources from spectool). The workflow
`.github/workflows/cernlib-rpm.yml` runs it for

- Fedora 45 (`fedora:45`): x86_64, i686 (mock in the x86_64 image: Fedora has no i686 image), aarch64, s390x (QEMU),
- CentOS Stream 9 and 10 (`quay.io/centos/centos:stream9`, `stream10`): x86_64, aarch64, s390x (QEMU; CentOS has no i686),

and publishes the RPMs of all of them as one dnf repository on GitHub Pages (`fedora/<release>/<arch>/`,
`centos/<release>/<arch>/`, one repository file per distribution; `$releasever` and `$basearch` are those of the machine):

    sudo dnf config-manager addrepo --from-repofile=https://andriish.github.io/JADE/JADE-fedora.repo
    sudo dnf config-manager addrepo --from-repofile=https://andriish.github.io/JADE/JADE-centos.repo   # CentOS

(one-time setting of the GitHub repository: Settings -> Pages -> Source: "GitHub Actions"). The repository holds
the packages of the last run, unsigned (`gpgcheck=0`). The workflow runs when `cernlib.spec` changes, and on request.

Local build of one architecture (the image of the distribution, e.g. `quay.io/centos/centos:stream9`):

    podman run --rm -v "$PWD":/work:Z -w /work fedora:45 bash .github/rpm/cernlib/build.sh x86_64 /work/out

To update the recipe copy the newest spec and patches from the `JADE` branch of HEPrpms.
