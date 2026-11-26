#!/bin/bash
# Build the CERNLIB RPMs inside the default image of the distribution (fedora:45, quay.io/centos/centos:stream9
# or stream10): the build dependencies are installed with dnf (on CentOS with EPEL and CRB enabled), the sources
# are downloaded with spectool, and rpmbuild builds the binary and the source packages.
#
#   podman run --rm [--platform linux/arm64] -v "$PWD":/work:Z -w /work fedora:45 \
#              bash .github/rpm/cernlib/build.sh <arch> /work/out
#
# <arch> is the architecture of the image: x86_64, aarch64, s390x (an image of that platform, e.g. under QEMU).
# i686 (Fedora only): Fedora has no i686 container image. The x86_64 image (started with --privileged) builds the source
# package, and mock builds it in a clean fedora-45-i386 root.
set -euo pipefail
arch=${1:?usage: build.sh <x86_64|aarch64|s390x|i686> <output directory>}
out=${2:?usage: build.sh <x86_64|aarch64|s390x|i686> <output directory>}
here=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$out"

# CentOS: the devel packages are in CRB, xbae, Motif and other packages in EPEL (like the CentOS Dockerfiles)
. /etc/os-release
if [ "$ID" != fedora ]; then
  dnf -y install dnf-plugins-core epel-release
  dnf config-manager --set-enabled crb
fi
dnf -y install rpm-build rpmdevtools 'dnf-command(builddep)' wget
top=$HOME/rpmbuild
mkdir -p "$top"/{BUILD,RPMS,SOURCES,SPECS,SRPMS}
cp "$here"/cernlib.spec "$top"/SPECS/
cp "$here"/*.patch "$here"/*.txt "$top"/SOURCES/
spectool -g -R "$top"/SPECS/cernlib.spec

if [ "$arch" = i686 ]; then
  dnf -y install mock
  rpmbuild -bs "$top"/SPECS/cernlib.spec
  cfg=fedora-45-i386
  [ -f /etc/mock/$cfg.cfg ] || cfg=fedora-rawhide-i386
  mock -r "$cfg" --isolation=simple --resultdir "$out" --rebuild "$top"/SRPMS/cernlib-*.src.rpm
  # the build logs of mock are not packages
  rm -f "$out"/*.log
else
  dnf -y builddep "$top"/SPECS/cernlib.spec
  rpmbuild -ba "$top"/SPECS/cernlib.spec
  cp "$top"/RPMS/*/*.rpm "$top"/SRPMS/*.src.rpm "$out"/
fi
ls -l "$out"
