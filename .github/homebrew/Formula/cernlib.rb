# Based on Formula/cernlib.rb of https://github.com/davidchall/homebrew-hep
# (commit fc557663284cb4e8287395cbc31151031c1d3b88).
# Changes: no bottle block (bottles are built by .github/workflows/homebrew-bottles.yml
# and the block is added by .github/homebrew/install.sh), CMAKE_POLICY_VERSION_MINIMUM for CMake 4,
# version 2026.11.01.1 (new download layout), install directories named after the version year.
class Cernlib < Formula
  desc "CERN library"
  homepage "https://cernlib.web.cern.ch"
  url "https://cernlib.web.cern.ch/download/2026_source/tar/cernlib-cernlib-2026.11.01.1-free.tar.gz"
  sha256 "093bd11997590ddb18af9e681bce4c319dcea0c79f2fa73d6a7b306edcf496d0"
  license "GPL-2.0-or-later"

  # Releases are in one directory per year: update the year when a new one starts.
  livecheck do
    url "https://cernlib.web.cern.ch/download/2026_source/tar/"
    regex(/href=.*?cernlib(?:-cernlib)?[._-](\d{4}(?:\.\d+)+)-free\.t/i)
  end

  option "with-test", "Test during installation"

  depends_on "cmake" => :build
  depends_on "gnu-sed" => :build
  depends_on "coreutils"
  depends_on "gcc"
  depends_on "libxaw"
  depends_on "openmotif"

  def install
    gcc_major_ver = Formula["gcc"].any_installed_version.major

    args = std_cmake_args + %W[
      -DCMAKE_POLICY_VERSION_MINIMUM=3.5
      -DCMAKE_Fortran_COMPILER=gfortran-#{gcc_major_ver}
      -DCMAKE_C_COMPILER=gcc-#{gcc_major_ver}
      -DMOTIF_INCLUDE_DIR=#{Formula["openmotif"].opt_include}
      -DMOTIF_LIBRARIES=#{Formula["openmotif"].opt_lib}/#{shared_library("libXm")}
      -DCMAKE_INSTALL_LIBDIR=lib/cernlib/#{version.major}/lib
      -DCMAKE_INSTALL_INCLUDEDIR=include/cernlib/#{version.major}
    ]

    mkdir "build" do
      system "xattr", "-rc", "../"
      system "cmake", "..", *args
      system "cmake", "--build", "."
      system "ctest" if build.with? "test"
      system "make", "install"
    end

    license_files = %w[
      GPL2.txt
      MCNETGUIDELINES.txt
      cernlib.copyright
    ]
    license_files.each { |file| cp "contrib/#{file}", prefix }
  end

  test do
    system "#{bin}/cernlib"
  end
end
