This directory contains a test suite for the JADE software. It is also the set of examples for the analysis of the JADE data
(ntuple based): the cards in ``cards``, the programs in ``src`` and the scripts show how to process real data and simulated events.
There are 46 tests; they depend on each other and have to be run as a chain (``sh jadetest.sh``):
 - ``CALIB_NAMES_aupdat1``, ``_bupdat0``, ``_bupdat1`` check that the names of the records of the calibration files are recognised by ``KLREAD`` (``src/calcheck.f``).
 - ``FPTOBOS_DATA_1_CLEANUP`` cleans up any remnants of the tests; ``FPTOBOS_DATA_1`` converts the JADE sample data into BOS format.
 - ``SUPERV_DATA_1``, ``ZE4V_DATA_1``, ``JZREAD_DATA_1``, ``JTJOB_DATA_1`` reconstruct the sample data, format the events and write HBOOK ntuples;
   ``H2ROOT_DATA_1`` converts the ntuple to ROOT, and ``ROOT_ANALYSIS_DATA_1``, ``_2`` analyse it (``src/ROOT_analysis_1.cc``, ``src/ROOT_analysis_2.cc``, the second with FastJet).
 - ``TAKLUMI_LUMITABLE_1`` runs ``taklumi`` with the installed ``lumi.dat`` and prints the table of the luminosity per range of the centre-of-mass energy.
 - ``PYTHIA8_PRODUCE_HEPMC3_MC_1`` produces events in HepMC format with Pythia8 (``src/pythia8_JADE.cc``).
 - ``HEPMC3_TO_CPROD_MC_1`` converts them into the CPROD format accepted by the JADE software.
 - ``MCJADE_MC_1`` produces BOS files from the CPROD file; ``SUPERV_MC_1``, ``ZE4V_MC_1``, ``JZREAD_MC_1``, ``JTJOB_MC_1``, ``H2ROOT_MC_1`` and the ROOT analyses follow.
 - ``PYTHIA8_PRODUCE_HEPMC3_MC_3`` ... ``ROOT_ANALYSIS2_MC_3`` repeat the Pythia8 chain at 43.8 GeV (``cards/testPythia2.input``, the detector configuration of 1985 in ``cards/mcjade3.card``).
 - ``PY57_MC_1_CLEANUP``, ``PY57_MC_1`` and the ``_MC_2`` chain use the old generator PYTHIA 5.7 of the JADE software.

  Please note that many tests require Fortran runtime that supports endianess conversion (e.g. ``GNU``, ``Intel`` or ``NVIDIA``).

  The directory ``cards`` contains all the steering cards for the used programs.
  The directory  ``src`` contains the source files of the Pythia8 main program, of the ROOT analyses and of the calibration check.
  The directory  ``jadetapes`` contains a small data file used for the tests.

The event display (`jadez`) is tested by three tests that need a virtual X display
(``xvfb-run`` or ``JADEZ_TEST_DISPLAY``; skipped otherwise). They are described in
``doc/EventDisplay.md``:
 - ``JADEZ_DATA_1`` runs 43 display commands (32 with a picture) on the real data sample (views, muon filter
   views, hardcopy after each drawing command) and checks the exit code, the hardcopies and the colours.
 - ``JADEZ_MC_1`` shows the reconstructed Monte Carlo event with the detector hardware, result tables,
   orthogonal projections and the dE/dx plot, and checks the thin grey chamber boundary lines.
 - ``JADEZ_RPHI_1`` produces the R-phi picture that mimics ``doc/eventdisplayreferences/2.jpg``.
 The scripts are ``jadez_test.sh``, ``jadez_mc_test.sh``, ``jadez_rphi_test.sh`` and ``check_colours.py``.
