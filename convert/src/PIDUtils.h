// -*- C++ -*-
//
// This file is part of MCUtils -- https://bitbucket.org/andybuckley/mcutils
// Copyright (C) 2013-2017 Andy Buckley <andy.buckley@cern.ch>
//
// Embedding of MCUtils code in other projects is permitted provided this
// notice is retained and the MCUtils namespace and include path are changed.
//
#ifndef MCUTILS_PIDUTILS_H
#define MCUTILS_PIDUTILS_H

/// @file Utility functions for querying PDG ID codes (many from HepPID)
/// @author Andy Buckley <andy.buckley@cern.ch>

//#include "HEPUtils/MathUtils.h"
#include "MathUtils.h"
//#include "MCUtils/PIDCodes.h"
#include "PIDCodes.h"

namespace MCUtils {
namespace PID {

using HEPUtils::in_closed_range;
using HEPUtils::in_range;


/// @defgroup mcutils_utils Utility functions
///@{

/// Absolute value
/// @deprecated Just use abs()!
inline int abspid(int pid) {
    return abs(pid);
}

///  PID digits (base 10) are: n nr nl nq1 nq2 nq3 nj
///  The Location enum provides a convenient index into the PID.
enum Location { nj=1, nq3, nq2, nq1, nl, nr, n, n8, n9, n10 };

/// Split the PID into constituent integers
inline unsigned short _digit(Location loc, int pid) {
    //  PID digits (base 10) are: n nr nl nq1 nq2 nq3 nj (cf. Location)
    int numerator = (int) std::pow(10.0, (loc-1));
    return (abs(pid)/numerator) % 10;
}

/// Returns everything beyond the 7th digit (e.g. outside the numbering scheme)
inline int _extraBits(int pid) {
    return abs(pid)/10000000;
}

/// @brief Return the first two digits if this is a "fundamental" particle
/// @note ID = 100 is a special case (internal generator ID's are 81-100)
inline int _fundamentalID(int pid) {
    if (_extraBits(pid) > 0) return 0;
    if (_digit(nq2,pid) == 0 && _digit(nq1,pid) == 0) {
        return abs(pid) % 10000;
    } else if (abs(pid) <= 100) {
        return abs(pid);
    } else {
        return 0;
    }
}

///@}


// Forward declaration
inline bool isBSM(int pid);


/// @defgroup mcutils_nucleus_ion Nucleus/ion functions
///@{

/// @brief Is this a nucleus PID?
///
/// This implements the 2006 Monte Carlo nuclear code scheme.
/// Ion numbers are +/- 10LZZZAAAI.
/// AAA is A - total baryon number
/// ZZZ is Z - total charge
/// L is the total number of strange quarks.
/// I is the isomer number, with I=0 corresponding to the ground state.
inline bool isNucleus(int pid) {
    // A proton can also be a Hydrogen nucleus
    if (abs(pid) == 2212) return true;
    // New standard: +/- 10LZZZAAAI
    if (_digit(n10,pid) == 1 && _digit(n9,pid) == 0) {
        // charge should always be less than or equal to baryon number
        // the following line is A >= Z
        if ((abs(pid)/10) % 1000 >= (abs(pid)/10000) % 1000) return true;
    }
    return false;
}

/// Get the atomic number (number of protons) in a nucleus/ion
/// @note Ion numbers are +/- 10LZZZAAAI.
inline int nuclZ(int pid) {
    // A proton can also be a Hydrogen nucleus
    if (abs(pid) == 2212) {
        return 1;
    }
    if (isNucleus(pid)) return (abs(pid)/10000) % 1000;
    return 0;
}
/// @deprecated Use nuclZ()
inline int Z(int pid) {
    return nuclZ(pid);
}

/// Get the atomic weight (number of nucleons) in a nucleus/ion
/// @note Ion numbers are +/- 10LZZZAAAI.
inline int nuclA(int pid) {
    // a proton can also be a Hydrogen nucleus
    if (abs(pid) == 2212) {
        return 1;
    }
    if (isNucleus(pid)) return (abs(pid)/10) % 1000;
    return 0;
}
/// @deprecated Use nuclA()
inline int A(int pid) {
    return nuclA(pid);
}

/// If this is a nucleus (ion), get nLambda
/// @note Ion numbers are +/- 10LZZZAAAI.
inline int nuclNlambda(int pid) {
    // a proton can also be a Hydrogen nucleus
    if (abs(pid) == 2212) {
        return 0;
    }
    if (isNucleus(pid)) return _digit(n8,pid);
    return 0;
}
/// @deprecated Use nuclNlambda()
inline int lambda(int pid) {
    return nuclNlambda(pid);
}

///@}


/// @defgroup mcutils_fundamental Fundamental particles
///@{

/// Determine if the PID is that of a quark
inline bool isQuark(int pid) {
    return in_closed_range(abs(pid), 1, 8);
}

/// Determine if the PID is that of a gluon
inline bool isGluon(int pid) {
    return pid == GLUON;
}

/// Determine if the PID is that of a parton (quark or gluon)
inline bool isParton(int pid) {
    return isGluon(pid) || isQuark(pid);
}


/// Determine if the PID is that of a photon
inline bool isPhoton(int pid) {
    return pid == PHOTON;
}

/// Determine if the PID is that of an electron or positron
inline bool isElectron(int pid) {
    return abs(pid) == ELECTRON;
}

/// Determine if the PID is that of an muon or antimuon
inline bool isMuon(int pid) {
    return abs(pid) == MUON;
}

/// Determine if the PID is that of an tau or antitau
inline bool isTau(int pid) {
    return abs(pid) == TAU;
}

/// Determine if the PID is that of a charged lepton
inline bool isChargedLepton(int pid) {
    const long apid = abs(pid);
    return apid == 11 || apid == 13 || apid == 15 || apid == 17;
}
/// Alias for isChargedLepton
/// @deprecated Prefer isChargedLepton
inline bool isChLepton(int pid) {
    return isChargedLepton(pid);
}

/// Determine if the PID is that of a neutrino
inline bool isNeutrino(int pid) {
    const long apid = abs(pid);
    return apid == 12 || apid == 14 || apid == 16 || apid == 18;
}


/// Determine if the PID is that of a W+
inline bool isWplus(int pid) {
    return pid == WPLUSBOSON;
}

/// Determine if the PID is that of a W-
inline bool isWminus(int pid) {
    return pid == WMINUSBOSON;
}

/// Determine if the PID is that of a W+-
inline bool isW(int pid) {
    return abs(pid) == WPLUSBOSON;
}

/// Determine if the PID is that of a Z0
inline bool isZ(int pid) {
    return pid == Z0BOSON;
}

/// Determine if the PID is that of an SM/lightest SUSY Higgs
inline bool isHiggs(int pid) {
    return pid == HIGGSBOSON || pid == 26; //< @todo Check on 26 still needed? (used in HERWIG SUSY, for example)
}

/// @todo isSUSYHiggs?


/// Is this a graviton?
inline bool isGraviton(int pid) {
    return pid == GRAVITON;
}


// /// Determine if the PID is that of a d/dbar
// inline bool isDown(int pid) { return abs(pid) == DQUARK; }

// /// Determine if the PID is that of a u/ubar
// inline bool isUp(int pid) { return abs(pid) == UQUARK; }

/// Determine if the PID is that of an s/sbar
inline bool isStrange(int pid) {
    return abs(pid) == SQUARK;
}

/// Determine if the PID is that of a c/cbar
inline bool isCharm(int pid) {
    return abs(pid) == CQUARK;
}

/// Determine if the PID is that of a b/bbar
inline bool isBottom(int pid) {
    return abs(pid) == BQUARK;
}

/// Determine if the PID is that of a t/tbar
inline bool isTop(int pid) {
    return abs(pid) == TQUARK;
}

///@}


/// @defgroup mcutils_qcomp Quark composite functions
///@{

/// Is this a pomeron, odderon, or generic reggeon?
inline bool isReggeon(int pid) {
    return pid == 110 || pid == 990 || pid == 9990;
}

/// Check to see if this is a valid meson
inline bool isMeson(int pid) {
    if (_extraBits(pid) > 0) return false;
    if (isBSM(pid)) return false;
    const int aid = abs(pid);
    if (aid == 130 || aid == 310 || aid == 210) return true; //< special cases for kaons
    if (aid <= 100) return false;
    if (_digit(nq1,pid) != 0) return false;
    if (_digit(nq2,pid) == 0) return false;
    if (_digit(nq3,pid) == 0) return false;
    if (_digit(nq2,pid) < _digit(nq3,pid)) return false;
    // EvtGen uses some odd numbers
    /// @todo Remove special-casing for EvtGen
    if (aid == 150 || aid == 350 || aid == 510 || aid == 530) return true;
    // Pomeron, Reggeon, etc.
    if (isReggeon(pid)) return false; //true; //< WTF?
    // Check for illegal antiparticles
    if (_digit(nj,pid) > 0 && _digit(nq3,pid) > 0 && _digit(nq2,pid) > 0 && _digit(nq1,pid) == 0) {
        return !(_digit(nq3,pid) == _digit(nq2,pid) && pid < 0);
    }
    return false;
}

/// Check to see if this is a valid baryon
inline bool isBaryon(int pid) {
    if (_extraBits(pid) > 0) return false;
    if (isBSM(pid)) return false;
    if (abs(pid) <= 100) return false;
    if (_fundamentalID(pid) <= 100 && _fundamentalID(pid) > 0) return false;
    if (abs(pid) == 2110 || abs(pid) == 2210) return true; ///< @todo Why this special case with nJ = 0? What are these? Not listed in RPP MC doc...
    if (_digit(nj,pid) == 0) return false;
    if (_digit(nq1,pid) == 0 || _digit(nq2,pid) == 0 || _digit(nq3,pid) == 0) return false;
    return true;
    /// @todo This is more correct by the definition, but the PDG's entries 1212, 1214, 1216, 1218 and 2122, 2124, 2126, 2128 come out as invalid
    // if ((_digit(nq1,pid) >= _digit(nq2,pid) && _digit(nq2,pid) >= _digit(nq3,pid)) ||
    //     (_digit(nq1,pid) > _digit(nq3,pid) && _digit(nq3,pid) > _digit(nq2,pid)) || //< case 6b for lighter quarks in J=1
    //     (_digit(nq3,pid) > _digit(nq1,pid) && _digit(nq1,pid) > _digit(nq2,pid))) //< case 6e for extra states in excited multiplets
    //   return true;
    // return false;
}

// Check to see if this is a valid diquark
inline bool isDiquark(int pid) {
    if (_extraBits(pid) > 0) return false;
    if (isBSM(pid)) return false;
    if (abs(pid) <= 100) return false;
    if (_fundamentalID(pid) <= 100 && _fundamentalID(pid) > 0) return false;
    if (_digit(nq1,pid) == 0) return false;
    if (_digit(nq2,pid) == 0) return false;
    if (_digit(nq3,pid) != 0) return false;
    if (_digit(nq1,pid) < _digit(nq2,pid)) return false;
    if (_digit(nj,pid) > 0  && _digit(nq3,pid) == 0 && _digit(nq2,pid) > 0 && _digit(nq1,pid) > 0) return true; // diquark signature
    // EvtGen uses the diquarks for quark pairs, so, for instance, 5501 is a valid "diquark" for EvtGen
    // if (_digit(nj) == 1 && _digit(nq2) == _digit(nq1)) {    // illegal
    //    return false;
    // } else {
    //  return true;
    // }
    return false;
}
/// @deprecated Use the nicer capitalisation isDiquark(pid)
inline bool isDiQuark(int pid) {
    return isDiquark(pid);
}

/// Check to see if this is a valid pentaquark
inline bool isPentaquark(int pid) {
    // a pentaquark is of the form 9abcdej,
    // where j is the spin and a, b, c, d, and e are quarks
    if (_extraBits(pid) > 0) return false;
    if (isBSM(pid)) return false;
    if (_digit(n,pid) != 9)  return false;
    if (_digit(nr,pid) == 9 || _digit(nr,pid) == 0)  return false;
    if (_digit(nj,pid) == 9 || _digit(nl,pid) == 0)  return false;
    if (_digit(nq1,pid) == 0)  return false;
    if (_digit(nq2,pid) == 0)  return false;
    if (_digit(nq3,pid) == 0)  return false;
    if (_digit(nj,pid) == 0)  return false;
    // check ordering
    if (_digit(nq2,pid) > _digit(nq1,pid))  return false;
    if (_digit(nq1,pid) > _digit(nl,pid))  return false;
    if (_digit(nl,pid) > _digit(nr,pid))  return false;
    return true;
}

/// Is this a valid hadron ID?
///
/// @note BSM hadrons, e.g. R-hadrons, don't count
inline bool isHadron(int pid) {
    if (_extraBits(pid) > 0) return false;
    if (isBSM(pid) > 0) return false;
    if (isMeson(pid)) return true;
    if (isBaryon(pid)) return true;
    if (isPentaquark(pid)) return true;
    return false;
}

///@}


/// @defgroup mcutils_idclasses More general particle class identification functions
///@{

/// Is this a valid lepton ID?
///
/// @note BSM "leptons" don't count
inline bool isLepton(int pid) {
    if (_extraBits(pid) > 0) return false;
    if (isBSM(pid) > 0) return false;
    if (_fundamentalID(pid) >= 11 && _fundamentalID(pid) <= 18) return true;
    return false;
}

/// Is this a valid BSM boson (SUSY Higgs, W', Z')?
inline bool isBSMBoson(int pid) {
    return in_closed_range(abs(pid), 32, 37);
}

/// Is this an SM fundamental particle?
inline bool isSMFundamental(int pid) {
    return isQuark(pid) || isLepton(pid) ||
           isGluon(pid) || isPhoton(pid) || isW(pid) || isZ(pid) || isHiggs(pid) ||
           isBSMBoson(pid) || isGraviton(pid);
}

/// @brief Is this a fundamental SUSY particle?
///
/// The MSSM extended Higgs sector is not counted as 'SUSY' particles, since they are not superpartners.
inline bool isSUSY(int pid) {
    // Fundamental SUSY particles have n = 1 or 2
    if (_extraBits(pid) > 0) return false;
    if (_digit(n,pid) != 1 && _digit(n,pid) != 2)  return false;
    if (_digit(nr,pid) != 0)  return false;
    // Check fundamental part for SM PID on which it is based
    const int fundId = _fundamentalID(pid);
    if (fundId == 0) return false;
    if (_digit(n,pid) == 1) { // most superpartners, incl LH sfermions
        return isSMFundamental(fundId);
    } else if (_digit(n,pid) == 2) { // RH sfermions
        return isQuark(fundId) || isChargedLepton(fundId);
    }
    return true;
}

/// Is this an R-hadron?
inline bool isRHadron(int pid) {
    // An R-hadron is of the form 10abcdj,
    // where j is the spin and a, b, c, and d are quarks or gluons
    if (_extraBits(pid) > 0) return false;
    if (_digit(n,pid) != 1)  return false;
    if (_digit(nr,pid) != 0)  return false;
    // Make sure this isn't a SUSY particle
    if (isSUSY(pid)) return false;
    // All R-hadrons have at least 3 core digits
    if (_digit(nq2,pid) == 0)  return false;
    if (_digit(nq3,pid) == 0)  return false;
    if (_digit(nj,pid) == 0)  return false;
    return true;
}
/// Alias
inline bool isRhadron(int pid) {
    return isRHadron(pid);
}

/// Is this a technicolor particle?
inline bool isTechnicolor(int pid) {
    if (_extraBits(pid) > 0) return false;
    return _digit(n,pid) == 3;
}

/// Is this an excited (composite) quark or lepton?
inline bool isExcited(int pid) {
    if (_extraBits(pid) > 0) return false;
    return _digit(n,pid) == 4 && _digit(nr,pid) == 0;
}

/// Is this a Kaluza-Klein excitation?
inline bool isKK(int pid) {
    if (_extraBits(pid) > 0) return false;
    const int ndigit = _digit(n,pid);
    return ndigit == 5 || ndigit == 6;
}

/// Is this a lepto-quark?
inline bool isLeptoQuark(int pid) {
    // Many UFO models are extending the PDG standards... is this going to be official?
    return abs(pid) == 42;
}

/// Is this a generic Dark Matter particle?
/// @note DM particles, including mediators, get the range 51-60
/// @note Also covers other cases: Heavy neutral leptons (50), Light pseudo-scalar A in 2HDM (55), Z' scalar UFO models (56)
/// @todo Give a more explicit name to clarify that this does not cover all DM particles, e.g. LSP?
inline bool isDarkMatter(int pid) {
    const int ndigit = _digit(n,pid);
    const int nrdigit = _digit(nr,pid);
    if ((ndigit == 0 && nrdigit == 0) || (ndigit == 5 && nrdigit == 9))
        return in_closed_range(abs(_fundamentalID(pid)),50,60);
    return false;
}
/// Convenience alias
inline bool isDM(int pid) {
    return isDarkMatter(pid);
}

/// Is this a Hidden Valley particle?
inline bool isHiddenValley(int pid) {
    return (_digit(n,pid) == 4 && _digit(nr,pid) == 9);
}

/// Is this an exotic particle?
inline bool isExotic(int pid) {
    // From the PDG definition, 40-80 reserved for exotic particles
    // Some overlap with ranges from other functions (e.g. isDM)
    // Also covers R0 (41)
    return in_closed_range(abs(pid),40,80);
}

/// Is this a 4th generation particle?
inline bool isFourthGen(int pid) {
    return abs(pid) == BPRIME || abs(pid) == TPRIME || abs(pid) == LPRIME || abs(pid) == NUPRIME;
}

/// Is this from a magnetic monopole or dyon?
inline bool isMagMonopole(int pid) {
    if (_digit(n,pid) != 4) return false;
    if (_digit(nr,pid) != 1) return false;
    if (_digit(nl,pid) != 1 && _digit(nl,pid) != 2) return false;
    // Require at least 1 core digit
    // NOT TRUE! Electrically neutral monopoles are possible
    // if (_digit(nq3,pid) == 0)  return false;
    // Always have spin zero for now
    if (_digit(nj,pid) != 0) return false;
    return true;
}
/// Just treat a dyon as an alias for magmonopole for now
inline bool isDyon(int pid) {
    return isMagMonopole(pid);
}

/// Is this a Q-ball?
/// @note Ad-hoc numbering for such particles is 100xxxx0, where xxxx is the charge in tenths.
inline bool isQBall(int pid) {
    if (_extraBits(pid) != 1) return false;
    if (_digit(n,pid) != 0) return false;
    if (_digit(nr,pid) != 0) return false;
    // Check the core number
    if ((abs(pid)/10) % 10000 == 0) return false;
    // These particles have spin zero for now
    if (_digit(nj,pid) != 0) return false;
    return true;
}
/// Alias
inline bool isQball(int pid) {
    return isQBall(pid);
}

/// Is this an excited lepton?
inline bool isExcitedLepton(int pid) {
    if (!isExcited(pid)) return false;
    return isLepton( _fundamentalID(pid) );
}

/// Is this a black hole?
inline bool isBlackHole(int pid) {
    if (_digit(n,pid) != 5 && _digit(n,pid) != 6) return false;
    if (_digit(nl,pid) != 0) return false;
    return _fundamentalID(pid)==40;
}

/// Is this an anomalously electrically charged particle (AECO)?
inline bool isAECO(int pid) {
    if (_digit( n,pid) != 1) return false;
    if (_digit(nr,pid) != 0) return false;
    if (_digit(nl,pid) != 0) return false;
    if (_digit(nj,pid) != 0) return false;
    return true;
}

/// Is this a BSM particle (including graviton)?
inline bool isBSM(int pid) {
    return isSUSY(pid) || isRHadron(pid) || isTechnicolor(pid) ||
           isExcited(pid) || isKK(pid) || isGraviton(pid) ||
           isBSMBoson(pid) || isLeptoQuark(pid) || isDM(pid) || isHiddenValley(pid) ||
           isExotic(pid) || isFourthGen(pid) || isBlackHole(pid) ||
           isDyon(pid) || isQball(pid) || isAECO(pid);
}

/// Check to see if this is a valid PID (i.e. matches any known scheme)
inline bool _isValid(int pid) {
    // Starting with 99 means anything goes (but nothing is known)
    if (_digit(n, pid) == 9 && _digit(nr, pid) == 9) return true;
    // Check that extra bits are only used for nuclei
    if (_extraBits(pid) > 0) return (isNucleus(pid) || isQball(pid));
    // Check that it fits into a standard non-nucleus convention
    if (isBSM(pid)) return true;
    if (isHadron(pid)) return true;
    if (_digit(n,pid) == 9 && _digit(nr,pid) == 0) return false; // could only have been a tentative hadron, but !isHadron
    if (isDiquark(pid)) return true;
    if (isPentaquark(pid)) return true;
    if (isReggeon(pid)) return true;
    // // Quark digit orderings required by the standard
    // if (_digit(nq1,pid) != 0 && _digit(nq1,pid) < _digit(nq2,pid)) return false;
    // if (_digit(nq2,pid) != 0 && _digit(nq2,pid) < _digit(nq3,pid)) return false;
    // Final check on fundamental ID
    return (_fundamentalID(pid) > 0);
}
inline bool isValid(int pid) {
    return _isValid(pid);
}

///@}


/// @defgroup mcutils_partoncontent Parton content functions
///@{

inline bool _hasQ(int pid, int q) {
    if (abs(pid) == q) return true; //< trivial case!
    if (!_isValid(pid)) return false;
    // if (_extraBits(pid) > 0) return false;
    // if (_fundamentalID(pid) > 0) return false;
    if (isMagMonopole(pid)) return false;
    if (isRHadron(pid)) {
        int iz = 7;
        for (int i = 6; i > 1; --i) {
            if (_digit(Location(i), pid) == 0) {
                iz = i;
            } else if ( i == iz-1 ) {
                // ignore squark or gluino
            } else {
                if (_digit(Location(i),pid) == q) return true;
            }
        }
        return false;
    }
    if (_digit(nq3,pid) == q || _digit(nq2,pid) == q || _digit(nq1,pid) == q ) return true;
    if (isPentaquark(pid)) {
        if (_digit(nl,pid) == q || _digit(nr,pid) == q) return true;
    }
    return false;
}

/// Does this particle contain a down quark?
inline bool hasDown(int pid) {
    return (isHadron(pid) || isQuark(pid)) && _hasQ(pid, 1);
}
/// Does this particle contain an up quark?
inline bool hasUp(int pid) {
    return (isHadron(pid) || isQuark(pid)) && _hasQ(pid, 2);
}
/// Does this particle contain a strange quark?
inline bool hasStrange(int pid) {
    return (isHadron(pid) || isQuark(pid)) && _hasQ(pid, 3);
}
/// Does this particle contain a charm quark?
inline bool hasCharm(int pid) {
    return (isHadron(pid) || isQuark(pid)) && _hasQ(pid, 4);
}
/// Does this particle contain a bottom quark?
inline bool hasBottom(int pid) {
    return (isHadron(pid) || isQuark(pid)) && _hasQ(pid, 5);
}
/// Does this particle contain a top quark?
inline bool hasTop(int pid) {
    return (isHadron(pid) || isQuark(pid)) && _hasQ(pid, 6);
}

///@}


/// @defgroup mcutils_parton_classes Parton content classification
///@{

/// Determine if the particle is a heavy flavour hadron or parton
inline bool isHeavyFlavor(int pid) {
    if (!isHadron(pid) && !isQuark(pid)) return false;
    return hasCharm(pid) || hasBottom(pid) || hasTop(pid);
}
/// British-spelling alias for isHeavyFlavor
inline bool isHeavyFlavour(int pid) {
    return isHeavyFlavor(pid);
}


// /// Determine if the particle is a light-flavour hadron or parton
// inline bool isLightFlavor(int pid) {
//   return !isHeavyFlavor();
// }


/// Determine if the PID is that of a heavy parton (c,b,t)
inline bool isHeavyParton(int pid) {
    return isParton(pid) && isHeavyFlavor(pid);
}

/// Determine if the PID is that of a light parton (u,d,s)
inline bool isLightParton(int pid) {
    return isParton(pid) && !isHeavyFlavor(pid);
}


/// Determine if the PID is that of a heavy flavour (b or c) meson
inline bool isHeavyMeson(int pid) {
    return isMeson(pid) && isHeavyFlavor(pid);
}

/// Determine if the PID is that of a heavy flavour (b or c) baryon
inline bool isHeavyBaryon(int pid) {
    return isBaryon(pid) && isHeavyFlavor(pid);
}

/// Determine if the PID is that of a heavy flavour (b or c) hadron
inline bool isHeavyHadron(int pid) {
    return isHadron(pid) && isHeavyFlavor(pid);
}

/// Determine if the PID is that of a light flavour (not b or c) meson
inline bool isLightMeson(int pid) {
    return isMeson(pid) && !isHeavyFlavor(pid);
}

/// Determine if the PID is that of a light flavour (not b or c) baryon
inline bool isLightBaryon(int pid) {
    return isBaryon(pid) && !isHeavyFlavor(pid);
}

/// Determine if the PID is that of a light flavour (not b or c) hadron
inline bool isLightHadron(int pid) {
    return isHadron(pid) && !isHeavyFlavor(pid);
}


/// Determine if the PID is that of a b-meson.
inline bool isBottomMeson(int pid) {
    return hasBottom(pid) && isMeson(pid);
}

/// Determine if the PID is that of a b-baryon.
inline bool isBottomBaryon(int pid) {
    return hasBottom(pid) && isBaryon(pid);
}

/// Determine if the PID is that of a b-hadron.
inline bool isBottomHadron(int pid) {
    return hasBottom(pid) && isHadron(pid);
}


/// @brief Determine if the PID is that of a c-meson.
///
/// @note Specifically, the _heaviest_ quark is a c: a B_c is a b-meson and NOT a c-meson.
/// Charmonia (closed charm) are counted as c-mesons here.
inline bool isCharmMeson(int pid) {
    return isMeson(pid) && hasCharm(pid) &&
           !hasBottom(pid);
}

/// @brief Determine if the PID is that of a c-baryon.
///
/// @note Specifically, the _heaviest_ quark is a c: a baryon containing a b & c
/// is a b-baryon and NOT a c-baryon. To test for the simpler case, just use
/// a combination of hasCharm() and isBaryon().
inline bool isCharmBaryon(int pid) {
    return isBaryon(pid) && hasCharm(pid) &&
           !hasBottom(pid);
}

/// Determine if the PID is that of a c-hadron.
///
/// @note Specifically, the _heaviest_ quark is a c: a baryon containing a b & c
/// is a b-baryon and NOT a c-baryon. To test for the simpler case, just use
/// a combination of hasCharm() and isBaryon().
inline bool isCharmHadron(int pid) {
    return isHadron(pid) && hasCharm(pid) &&
           !hasBottom(pid);
}


/// Determine if the PID is that of a strange meson
///
/// @note Specifically, the _heaviest_ quark is an s: if it also contains
/// either charm or bottom, it is not considered to be a strange hadron.
inline bool isStrangeMeson(int pid) {
    return isMeson(pid) && hasStrange(pid) &&
           !(hasBottom(pid) || hasCharm(pid));
}

/// Determine if the PID is that of a strange baryon
///
/// @note Specifically, the _heaviest_ quark is an s: if it also contains
/// either charm or bottom, it is not considered to be a strange hadron.
inline bool isStrangeBaryon(int pid) {
    return isBaryon(pid) && hasStrange(pid) &&
           !(hasBottom(pid) || hasCharm(pid));
}

/// Determine if the PID is that of a strange hadron
///
/// @note Specifically, the _heaviest_ quark is an s: if it also contains
/// either charm or bottom, it is not considered to be a strange hadron.
inline bool isStrangeHadron(int pid) {
    return isHadron(pid) && hasStrange(pid) &&
           !(hasBottom(pid) || hasCharm(pid));
}

///@}



/// @defgroup mcutils_angmom Angular momentum functions
///@{

/// jSpin returns 2J+1, where J is the total spin
inline int jSpin(int pid) {
    const int fund = _fundamentalID(pid);
    if (fund > 0) {
        // some of these are known
        if (fund > 0 && fund < 7) return 2;
        if (fund == 9) return 3;
        if (fund > 10 && fund < 17) return 2;
        if (fund > 20 && fund < 25) return 3;
        return 0;
    } else if (_extraBits(pid) > 0) {
        return 0;
    }
    return abs(pid) % 10;
}

/// sSpin returns 2S+1, where S is the spin
inline int sSpin(int pid) {
    // Handle invalid cases first
    if (!isMeson(pid)) return 0;
    if (_digit(n,pid) == 9 && _digit(nr,pid) == 0) return 0; // tentative ID
    // Special generic DM particles with defined spins
    const int fund = _fundamentalID(pid);
    if (fund == 51 || fund == 54) return 1;
    if (fund == 52) return 2;
    if (fund == 53 || fund == 55) return 3;
    // Calculate from nl and nj digits
    const int inl = _digit(nl,pid);
    const int js = _digit(nj,pid);
    if (inl == 0 && js >= 3) return 1;
    else if (inl == 0  && js == 1) return 0;
    else if (inl == 1  && js >= 3) return 0;
    else if (inl == 2  && js >= 3) return 1;
    else if (inl == 1  && js == 1) return 1;
    else if (inl == 3  && js >= 3) return 1;
    // Default to zero
    return 0;
}

/// lSpin returns 2L+1, where L is the orbital angular momentum
inline int lSpin(int pid) {
    // Handle invalid cases first
    if (!isMeson(pid)) return 0;
    if (_digit(n,pid) == 9 && _digit(nr,pid) == 0) return 0; // tentative ID
    // Calculate from nl and nj digits
    const int inl = _digit(nl,pid);
    const int js = _digit(nj,pid);
    if (inl == 0 && js == 3) return 0;
    else if (inl == 0 && js == 5) return 1;
    else if (inl == 0 && js == 7) return 2;
    else if (inl == 0 && js == 9) return 3;
    else if (inl == 0 && js == 1) return 0;
    else if (inl == 1 && js == 3) return 1;
    else if (inl == 1 && js == 5) return 2;
    else if (inl == 1 && js == 7) return 3;
    else if (inl == 1 && js == 9) return 4;
    else if (inl == 2 && js == 3) return 1;
    else if (inl == 2 && js == 5) return 2;
    else if (inl == 2 && js == 7) return 3;
    else if (inl == 2 && js == 9) return 4;
    else if (inl == 1 && js == 1) return 1;
    else if (inl == 3 && js == 3) return 2;
    else if (inl == 3 && js == 5) return 3;
    else if (inl == 3 && js == 7) return 4;
    else if (inl == 3 && js == 9) return 5;
    // Default to zero
    return 0;
}

///@}


/// @defgroup mcutils_charge Charge functions
///@{

/// Three times the EM charge (as integer)
inline int charge3(int pid) {
    static int ch100[100] = { -1,  2, -1, 2, -1, 2, -1, 2, 0, 0,
                              -3,  0, -3, 0, -3, 0, -3, 0, 0, 0,
                              0,  0,  0, 3,  0, 0,  0, 0, 0, 0,
                              0,  0,  0, 3,  0, 0,  3, 0, 0, 0,
                              0, -1,  0, 0,  0, 0,  0, 0, 0, 0,
                              0,  6,  3, 6,  0, 0,  0, 0, 0, 0,
                              0,  0,  0, 0,  0, 0,  0, 0, 0, 0,
                              0,  0,  0, 0,  0, 0,  0, 0, 0, 0,
                              0,  0,  0, 0,  0, 0,  0, 0, 0, 0,
                              0,  0,  0, 0,  0, 0,  0, 0, 0, 0
                            };
    const unsigned short q1 = _digit(nq1,pid);
    const unsigned short q2 = _digit(nq2,pid);
    const unsigned short q3 = _digit(nq3,pid);
    const unsigned short ql = _digit(nl,pid);
    const int ida = abs(pid);
    const int sid = _fundamentalID(pid);
    int ch3 = 0;
    if (ida == 0 || _extraBits(pid) > 0) { // ion or illegal
        return 0;
    } else if( isQBall(pid) ) { // QBall
        ch3 = 3*( (ida/10) % 10000);
    } else if( isHiddenValley(pid) ) { // Hidden Valley
        return 0;
    } else if( isDyon(pid) ) { // Dyon
        ch3 = 3*( (ida/10) %  1000) * (ql == 2 ? -1 : 1); //< NB. charge is flipped at the end if pid < 0
    } else if (sid > 0 && sid <= 100) { // Use table
        if (ida == 1000017 || ida == 1000018 || ida == 1000034) ch3 = 0;
        else if (ida > 1000050 && ida <= 1000060) ch3 = 0; // ?
        else if (ida > 50 && ida <= 60) ch3 = 0; // Generic DM
        else if (ida == 5100061 || ida == 5100062) ch3 = 6;
        else ch3 = ch100[sid-1];
    } else if (_digit(nj,pid) == 0) { // KL, Ks, or undefined
        return 0;
    } else if (isMeson(pid)) { // Mesons
        ch3 = ((q2 == 3 || q2 == 5) ? -1 : 1) * (ch100[q2-1] - ch100[q3-1]);
    } else if (isRHadron(pid) ) { // R-hadron
        /// @todo Is this sufficiently general? Why only gluino in g+q+qbar? Better to recurse to the related SM hadron code?
        if (q1 == 0 || q1 == 9) { //< gluino+q+qbar
            if (q2 == 3 || q2 == 5) {
                ch3 = ch100[q3-1] - ch100[q2-1];
            } else {
                ch3 = ch100[q2-1] - ch100[q3-1];
            }
        } else if (ql == 0) { //< squark+q+q
            ch3 = ch100[q3-1] + ch100[q2-1] + ch100[q1-1];
        } else if (_digit(nr,pid) == 0) { //< squark+q+q+q
            ch3 = ch100[q3-1] + ch100[q2-1] + ch100[q1-1] + ch100[ql-1];
        }
    } else if (isDiQuark(pid)) { // Diquarks
        ch3 = ch100[q2-1] + ch100[q1-1];
    } else if (isBaryon(pid)) { // Baryons
        ch3 = ch100[q3-1] + ch100[q2-1] + ch100[q1-1];
    } else { // Unknown
        return 0;
    }
    if (pid < 0) ch3 *= -1;
    return ch3;
}

/// Alias for charge3
/// @deprecated Prefer charge3
inline int threeCharge(int pid) {
    return charge3(pid);
}

/// Return the absolute value of 3 times the EM charge
inline int abscharge3(int pid) {
    return std::abs(charge3(pid));
}

/// Return the EM charge (as floating point)
inline double charge(int pid) {
    return charge3(pid)/3.0;
}

/// Return the EM charge (as floating point)
inline double abscharge(int pid) {
    return std::abs(charge(pid));
}

///@}


/// @defgroup mcutils_charge_classes General PID-based classifier functions
///@{

/// Determine if the particle is electrically charged
inline bool isCharged(int pid) {
    return charge3(pid) != 0;
}

/// Determine if the particle is electrically neutral
inline bool isNeutral(int pid) {
    return charge3(pid) == 0;
}

///@}


/// @defgroup mcutils_interactions Interaction classifiers
///@{

/// Determine if the PID is that of a strongly interacting particle
inline bool isStrongInteracting(int pid) {
    return isParton(pid) || isHadron(pid);
}

/// Determine if the PID is that of a electromagnetically interacting particle
inline bool isEMInteracting(int pid) {
    return isCharged(pid) || isPhoton(pid);
}

/// Determine if the PID is that of a weakly interacting particle
///
/// @note Photons are considered weak-interacting, as are all hadrons and
/// leptons (we can't distinguish between L and R fermions at physical particle level).
inline bool isWeakInteracting(int pid) {
    return !isGluon(pid) && !isGraviton(pid);
}

///@}


/// @defgroup mcutils_other Other classifiers
///@{

/// Determine if the PID is in the generator-specific range
inline bool isGenSpecific(int pid) {
    return in_range(pid, 80, 101);
}

/// Determine if the PID is that of an EW scale resonance
///
/// @todo Also include SUSY, technicolor, etc. etc.? Maybe via a isStandardModel(pid) function, but there are stable BSM particles (in principle)
inline bool isResonance(int pid) {
    return isW(pid) || isZ(pid) || isHiggs(pid) || isTop(pid);
}

/// Check the PID for usability in transport codes like Geant4
///
/// @todo Should exclude neutrinos/LSP, since the ATLAS G4 interface deletes them immediately?
/// @todo What about long-lived particles... could be BSM but need to be transported
inline bool isTransportable(int pid) {
    // return !isResonance(pid) && !isParton(pid) && !isGenSpecific(pid);
    return isPhoton(pid) || isHadron(pid) || isLepton(pid);
}

///@}


}
}

#endif
