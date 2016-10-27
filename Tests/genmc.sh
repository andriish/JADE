#!/bin/bash
export LD_LIBRARY_PATH=//usr/lib64/SHERPA-MC/://usr/lib64/SHERPA-MC/:$LD_LIBRARY_PATH
Sherpa -f Runeesherpa_0.12.dat EVENT_OUTPUT=HepMC_GenEvent[out.hepmc2] HEPMC_TREE_LIKE=1  EVENTS=10000
mv out.hepmc2.hepmc2g   sherpa34gev.hepmc2
../HepMC3/outputs/bin/convert_example_JADE.exe hepmc2_jade  sherpa34gev.hepmc2   sherpa34gev.jade
cat mcjadecard.txt | mcjade
