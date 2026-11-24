/**
 * Dummy generator of HepMC3 events e+ e- -> two particles (pi+ pi-, mu+ mu-, K0S K0S, ...), back to back in the
 * centre-of-mass system (the beams collide head on, no radiation, the vertex is at the origin). The angle is
 * uniform in cos(theta) between -cosmax and +cosmax, the azimuth is uniform. The events are for debugging the
 * chain of the programs (HepMC3 -> CPROD -> MCJADE -> SUPERV -> analysis): the momenta of the particles are
 * known (the beam energy).
 *
 * usage: dummy_gen <output HepMC3 file> <number of events> <centre-of-mass energy, GeV> <PDG code of the particle> [cosmax]
 *   The two particles are the particle and its antiparticle (a particle that is its own antiparticle, such as
 *   the K0S 310, is produced twice). A K0S decays to pi+ pi- (isotropic in its rest frame) after a flight
 *   with the proper lifetime (c tau = 26.8 mm): the pions are produced at the decay point, i.e. the sample has
 *   displaced vertices and known K0S, for the test of the K0S analysis.
 */
#include "HepMC3/GenEvent.h"
#include "HepMC3/GenParticle.h"
#include "HepMC3/GenVertex.h"
#include "HepMC3/WriterAscii.h"
#include "HepMC3/Print.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <map>
#include <random>

using namespace HepMC3;

static double mass_of(int pdg) {
    static const std::map<int, double> m = {{11, 0.000511}, {13, 0.105658}, {211, 0.139570}, {321, 0.493677},
                                            {310, 0.497611}, {130, 0.497611}, {2212, 0.938272}, {22, 0.0}};
    auto it = m.find(std::abs(pdg));
    if (it == m.end()) {
        std::cerr << "dummy_gen: unknown particle " << pdg << " (11, 13, 22, 130, 211, 310, 321, 2212)" << std::endl;
        std::exit(2);
    }
    return it->second;
}

// boost of the four-vector q by the velocity (bx,by,bz) (c = 1)
static FourVector boost(const FourVector& q, double bx, double by, double bz) {
    const double b2 = bx * bx + by * by + bz * bz;
    if (b2 <= 0) return q;
    const double g = 1.0 / std::sqrt(1.0 - b2);
    const double bp = bx * q.px() + by * q.py() + bz * q.pz();
    const double g2 = (g - 1.0) / b2;
    return FourVector(q.px() + g2 * bp * bx + g * bx * q.e(), q.py() + g2 * bp * by + g * by * q.e(),
                      q.pz() + g2 * bp * bz + g * bz * q.e(), g * (q.e() + bp));
}

int main(int argc, char** argv) {
    if (argc < 5) {
        std::cout << "Usage: " << argv[0] << " <output HepMC3 file> <number of events> <centre-of-mass energy, GeV> <PDG code> [cosmax]"
                  << std::endl;
        return 1;
    }
    const int nev = std::atoi(argv[2]);
    const double ecm = std::atof(argv[3]);
    const int pdg = std::atoi(argv[4]);
    const double cosmax = argc > 5 ? std::atof(argv[5]) : 0.8;
    const double m = mass_of(pdg);
    const double e = ecm / 2.0;
    if (e <= m) {
        std::cerr << "dummy_gen: the energy is below the threshold" << std::endl;
        return 2;
    }
    const double p = std::sqrt(e * e - m * m);
    const int pdg2 = (pdg == 310 || pdg == 130 || pdg == 22) ? pdg : -pdg;
    std::mt19937 rng(12345);
    std::uniform_real_distribution<double> uc(-cosmax, cosmax), uf(0.0, 2.0 * M_PI);

    std::shared_ptr<GenRunInfo> run = std::make_shared<GenRunInfo>();
    struct GenRunInfo::ToolInfo tool = {std::string("dummy_gen"), "1.0", std::string("e+e- -> two particles")};
    run->tools().push_back(tool);
    run->set_weight_names({"default"});
    WriterAscii file(argv[1], run);

    for (int i = 0; i < nev; ++i) {
        GenEvent evt(Units::GEV, Units::MM);
        evt.set_event_number(i + 1);
        // the beams, status 4 (as for Pythia8); the electron goes along +z
        auto b1 = std::make_shared<GenParticle>(FourVector(0, 0, e, e), 11, 4);
        auto b2 = std::make_shared<GenParticle>(FourVector(0, 0, -e, e), -11, 4);
        const double c = uc(rng), s = std::sqrt(1.0 - c * c), f = uf(rng);
        const double px = p * s * std::cos(f), py = p * s * std::sin(f), pz = p * c;
        const bool decay = (pdg == 310);
        auto p1 = std::make_shared<GenParticle>(FourVector(px, py, pz, e), pdg, decay ? 2 : 1);
        auto p2 = std::make_shared<GenParticle>(FourVector(-px, -py, -pz, e), pdg2, decay ? 2 : 1);
        auto v = std::make_shared<GenVertex>();
        v->add_particle_in(b1);
        v->add_particle_in(b2);
        v->add_particle_out(p1);
        v->add_particle_out(p2);
        evt.add_vertex(v);
        if (decay) {
            const double mpi = 0.139570, ctau = 26.8;  // mm
            const double ps = std::sqrt(0.25 * m * m - mpi * mpi);
            std::exponential_distribution<double> life(1.0 / ctau);
            std::uniform_real_distribution<double> u1(-1.0, 1.0);
            for (int k = 0; k < 2; ++k) {
                const double sgn = k == 0 ? 1.0 : -1.0;
                const double bx = sgn * px / e, by = sgn * py / e, bz = sgn * pz / e;
                // flight length: proper decay time times beta gamma
                const double len = life(rng) * (p / m);
                const double dx = sgn * px / p * len, dy = sgn * py / p * len, dz = sgn * pz / p * len;
                const double cc = u1(rng), ss = std::sqrt(1.0 - cc * cc), ff = uf(rng);
                FourVector q1(ps * ss * std::cos(ff), ps * ss * std::sin(ff), ps * cc, 0.5 * m);
                FourVector q2(-q1.px(), -q1.py(), -q1.pz(), 0.5 * m);
                auto dv = std::make_shared<GenVertex>(FourVector(dx, dy, dz, len * e / p));
                dv->add_particle_in(k == 0 ? p1 : p2);
                dv->add_particle_out(std::make_shared<GenParticle>(boost(q1, bx, by, bz), 211, 1));
                dv->add_particle_out(std::make_shared<GenParticle>(boost(q2, bx, by, bz), -211, 1));
                evt.add_vertex(dv);
            }
        }
        if (i == 0) Print::listing(evt);
        file.write_event(evt);
    }
    file.close();
    return 0;
}
