# Based on Formula/hepmc3.rb of https://github.com/davidchall/homebrew-hep.
# Changes: version 3.3.1, no bottle block (bottles are built by
# .github/workflows/homebrew-bottles.yml and the block is added by .github/homebrew/install.sh),
# no Python bindings and no ROOT I/O (not needed by JADE), search library enabled,
# examples neither built nor installed (the test compiles a small program instead).
class Hepmc3 < Formula
  desc "C++ event record for Monte Carlo generators"
  homepage "https://hepmc.web.cern.ch/hepmc/"
  url "https://hepmc.web.cern.ch/hepmc/releases/HepMC3-3.3.1.tar.gz"
  sha256 "08240160b0f28dc3293aa4d61ce65e2d67cd597acf6faca439f2e46625f7e793"
  license "LGPL-3.0-or-later"

  livecheck do
    url "https://hepmc.web.cern.ch/hepmc/"
    regex(/href=.*?HepMC3[._-]v?(\d+(?:\.\d+)+)\.t/i)
  end

  option "with-test", "Test during installation"

  depends_on "cmake" => [:build, :test]
  depends_on "coreutils" # HepMC3-config uses greadlink

  def install
    args = %w[
      -DHEPMC3_INSTALL_INTERFACES=ON
      -DHEPMC3_BUILD_STATIC_LIBS=OFF
      -DHEPMC3_ENABLE_SEARCH=ON
      -DHEPMC3_ENABLE_PYTHON=OFF
      -DHEPMC3_ENABLE_ROOTIO=OFF
      -DHEPMC3_ENABLE_PROTOBUFIO=OFF
      -DHEPMC3_BUILD_EXAMPLES=OFF
      -DHEPMC3_INSTALL_EXAMPLES=OFF
    ]
    args << "-DHEPMC3_ENABLE_TEST=ON" if build.with? "test"

    system "cmake", "-S", ".", "-B", "build", *std_cmake_args, *args
    system "cmake", "--build", "build"
    system "ctest", "--test-dir", "build", "--output-on-failure" if build.with? "test"
    system "cmake", "--install", "build"
  end

  test do
    assert_equal prefix.to_s, shell_output("#{bin}/HepMC3-config --prefix").strip

    (testpath/"t.cc").write <<~EOS
      #include <HepMC3/GenEvent.h>
      #include <HepMC3/GenParticle.h>
      #include <HepMC3/GenVertex.h>
      #include <HepMC3/Print.h>
      int main() {
        HepMC3::GenEvent evt(HepMC3::Units::GEV, HepMC3::Units::MM);
        auto v = std::make_shared<HepMC3::GenVertex>();
        evt.add_vertex(v);
        v->add_particle_out(std::make_shared<HepMC3::GenParticle>(
            HepMC3::FourVector(0, 0, 1, 1), 22, 1));
        return evt.particles().size() == 1 ? 0 : 1;
      }
    EOS
    system ENV.cxx, "-std=c++17", "t.cc", "-I#{include}", "-L#{lib}", "-lHepMC3", "-o", "t"
    system "./t"
  end
end
