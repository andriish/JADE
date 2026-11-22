Directory with code to convert different formats of MC events, e.g. 
HepMC/HepMC3 into JADE format. The utility in this directory requires 
HepMC3>3.1.0 library and it's code is based on the example from the HepMC3 
library (or vice verse).

## Third-party code

`src/AtlasPID.h`: PDG particle ID utilities (PDG 2022 numbering, used for the particle charge), copied from https://gitlab.cern.ch/averbyts/atlaspid (Andrii Verbytskyi). It replaces the MCUtils/HEPUtils headers used before.
