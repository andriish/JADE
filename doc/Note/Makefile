latexfile=JADE2026
newname?=FFC
STYLE?=draft

LATEX=pdflatex -halt-on-error -shell-escape
BIBTEX=bibtex


ifeq ($(STYLE),draft)
	STYLE_SPECIFIC_FILES=zeus_paper.cls $(latexfile)-draft.tex
endif

ifeq ($(STYLE),arxiv)
	STYLE_SPECIFIC_FILES=zeus_paper.cls $(latexfile)-arxiv.tex
endif

ifeq ($(STYLE),epjc)
	STYLE_SPECIFIC_FILES=svjour.cls  svepj.clo $(latexfile)-epjc.tex
endif


all: $(latexfile)-$(STYLE).pdf

tar:  Makefile $(figures) $(constanttables) $(tables) $(latexfile).bst $(latexfile).bib $(latexfile)-abs.tex $(latexfile)-txt.tex $(latexfile)-tab.tex $(latexfile)-fig.tex  $(STYLE_SPECIFIC_FILES)
	rm -rf TAR
	mkdir -p TAR
	mkdir -p TAR/Figures
	mkdir -p TAR/Tables
	mkdir -p TAR/ConstantTables
	cp Makefile  $(latexfile).bst $(latexfile).bib $(latexfile)-abs.tex $(latexfile)-txt.tex $(latexfile)-tab.tex $(latexfile)-fig.tex    $(STYLE_SPECIFIC_FILES) TAR
	cp $(constanttables) TAR/ConstantTables
	cp $(tables) TAR/Tables
	cp $(figures) TAR/Figures
	sed -i 's@'$(latexfile)'@'$(newname)'@g' TAR/*tex	
	sed -i 's@'$(latexfile)'@'$(newname)'@g' TAR/Makefile
	sed -i 's@^STYLE.*@STYLE='$(STYLE)'@g' TAR/Makefile
	sed -i '/@artic/achecked={2},'     TAR/*bib	
	sed -i '/@inproceed/achecked={2},' TAR/*bib	
	sed -i '/@phdthesis/achecked={2},' TAR/*bib	

ifeq ($(STYLE),epjc)
	sed -i 's@\[htbp\]@@g' TAR/$(latexfile)-tab.tex
	sed -i 's@c|@c@g' TAR/$(latexfile)-tab.tex
	sed -i 's@|c@c@g' TAR/$(latexfile)-tab.tex
	sed -i 's@}|@}@g' TAR/$(latexfile)-tab.tex
endif
	
	mv TAR/$(latexfile).bst TAR/$(newname).bst 
	mv TAR/$(latexfile).bib TAR/$(newname).bib 
	mv TAR/$(latexfile)-$(STYLE).tex TAR/$(newname)-$(STYLE).tex
	mv TAR/$(latexfile)-txt.tex TAR/$(newname)-txt.tex
	mv TAR/$(latexfile)-abs.tex TAR/$(newname)-abs.tex
	mv TAR/$(latexfile)-tab.tex TAR/$(newname)-tab.tex
	mv TAR/$(latexfile)-fig.tex TAR/$(newname)-fig.tex
ifeq ($(STYLE),arxiv)
	#####-->A trick to get the bib file for axiv build system
	$(MAKE) -C TAR
	mv TAR/$(newname)-$(STYLE).bbl  TAR/$(newname)-$(STYLE).bbl.temp
	$(MAKE) -C TAR clean
	mv TAR/$(newname)-$(STYLE).bbl.temp  TAR/$(newname)-$(STYLE).bbl
endif
	tar -zcvf   $(newname).tar.gz -C TAR . 
	rm -f $(newname).zip
	cd TAR; zip -r ../$(newname).zip  ./*
	rm -rf TAR


figures=
#Figures/figure0.eps  Figures/figure0.pdf\
#        Figures/figure1.eps  Figures/figure1.pdf 
tables=
#Tables/constants.txt Tables/constants_lhcb13.txt\
 #      Tables/ee10_tab.txt Tables/eez0_tab.txt Tables/heradis_tab.txt\
  #     Tables/heraphp_tab.txt Tables/lhcb_tab.txt Tables/average_tab2.txt\
   #    Tables/average_cov.txt Tables/excited_tab.txt Tables/pdg_tab.txt
constanttables=
#ConstantTables/pp_ALICE_7TeV.txt ConstantTables/pp_ATLAS_7TeV.txt\
 #              ConstantTables/av_David.txt ConstantTables/pp_CDF_2TeV.txt
#

$(latexfile)-$(STYLE).pdf :  $(figures) $(constanttables) $(tables) $(latexfile).bst\
                    $(latexfile).bib $(latexfile)-abs.tex $(latexfile)-txt.tex $(latexfile)-tab.tex\
                     $(latexfile)-fig.tex   $(STYLE_SPECIFIC_FILES)
	
	$(LATEX) $(latexfile)-$(STYLE)
	$(LATEX) $(latexfile)-$(STYLE)
	$(BIBTEX) $(latexfile)-$(STYLE)
	$(BIBTEX) $(latexfile)-$(STYLE)
ifeq ($(STYLE),epjc)
	sed -i 's@\\mbox{}.*@@g'  $(latexfile)-$(STYLE).bbl
endif
ifeq ($(STYLE),arxiv)
	sed -i 's@\\mbox{}.*@@g'  $(latexfile)-$(STYLE).bbl
endif
	$(LATEX) $(latexfile)-$(STYLE)
	$(LATEX) $(latexfile)-$(STYLE)

pdf : $(latexfile)-$(STYLE).pdf


$(latexfile).rws: $(latexfile)-dict.txt
	aspell --lang=en create master ./$(latexfile).rws < ./$(latexfile)-dict.txt

spell: $(latexfile).rws
		for a in $$(ls -1 $(latexfile)-*.tex); do\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		echo $$a;\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		cat $$a |aspell  list --add-tex-command="app p" --add-tex-command="label p" --add-tex-command="eq p" --add-tex-command="usetikzlibrary p" --add-tex-command="sect p" --add-tex-command="fig p" --add-tex-command="tab p"  --add-extra-dicts=./$(latexfile).rws  -t --dont-tex-check-comments -d en_GB ;\
		done

		for a in $$(ls -1 $(latexfile)-*.tex); do\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		echo $$a;\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		aspell   --add-tex-command="label p" --add-tex-command="usetikzlibrary p" --add-tex-command="sect p" --add-tex-command="fig p" --add-tex-command="tab p"  --add-extra-dicts=./$(latexfile).rws  -t --dont-tex-check-comments -d en_GB -c $$a ;\
		done

########################################################################
# Figures made by the programs of the test suite (event display, thrust, K0S).
#   make figures
# runs the chains of the tests with the cards of test/cards in its own work directory ($(WORK)), i.e. the
# output of ctest is not used, and makes the PDF files in Figures/ (which are kept in the repository).
# It needs the installed JADESOFT (jadeinstall.sh), the executables of the test directory (pythia8_JADE, k0s,
# ROOT_analysis_1: jadetest.sh builds them), ROOT, a virtual X server for jadez (xvfb-run or JADEZ_TEST_DISPLAY),
# and ps2eps, gs, convert. The margins of the ROOT pictures are cut in the note (\adjincludegraphics of adjustbox). The settings are for gfortran (GFORTRAN_CONVERT_UNIT).
#   make figures JADE_INSTALL=/path/to/installedGNU64 JADE_BUILD=/path/to/buildGNU64/test
JADE_INSTALL ?= ../../installedGNU64
JADE_BUILD   ?= ../../buildGNU64/test
TESTDIR      := $(abspath ../../test)
INST         := $(abspath $(JADE_INSTALL))
BLD          := $(abspath $(JADE_BUILD))
WORK         ?= work
W            := $(abspath $(WORK))
BIN          := $(INST)/bin
DATADIR      := $(INST)/share/JADESOFT
H2ROOT       ?= $(firstword $(wildcard $(BIN)/jade_h2root) $(shell command -v h2root))
ROOTLIB      := $(shell root-config --libdir 2>/dev/null)
FIG          := Figures

# $(call RUN,<environment>,<card>,<program>): the program reads the card from the standard input
define RUN
cd $(W) && env $(1) LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH sh -c "cat $(TESTDIR)/cards/$(2) | $(3)" > $(basename $(notdir $(2)))_$(notdir $(3)).log 2>&1
endef
NAT  := GFORTRAN_CONVERT_UNIT=native
BE   := GFORTRAN_CONVERT_UNIT=big_endian
SUP  := 'GFORTRAN_CONVERT_UNIT=big_endian;native:2'
JTJ  := 'GFORTRAN_CONVERT_UNIT=native;big_endian:2,22'
K0SE := 'GFORTRAN_CONVERT_UNIT=native;big_endian:2'

.PHONY: figures clean-work
figures: $(FIG)/display_ref1.pdf $(FIG)/display_ref2.pdf $(FIG)/display_rphi.pdf $(FIG)/display_mc_rc1.pdf \
         $(FIG)/thrust_data.pdf $(FIG)/thrust_mc1.pdf $(FIG)/thrust_mc2.pdf $(FIG)/thrust_mc3.pdf \
         $(FIG)/energy_data.pdf $(FIG)/energy_mc1.pdf $(FIG)/energy_mc2.pdf $(FIG)/energy_mc3.pdf \
         $(FIG)/k0s_mass.pdf $(FIG)/k0s_cuts.pdf $(FIG)/k0s_event.pdf $(FIG)/k0s_energy.pdf \
         $(FIG)/k0s_mass_lept.pdf $(FIG)/k0s_cuts_lept.pdf $(FIG)/k0s_event_lept.pdf $(FIG)/k0s_energy_lept.pdf

clean-work:
	rm -rf $(W)

# work directory: calibration files and the sample of the data tapes
# the real data: the large file JAD146/file001 of the tapes (47484 events, not in the repository; copy it to test/jadetapes/JAD146/file001
# from the archive), or the small sample file of the repository if it is not there. The cards of the data chain read the file as
# jadetapes/JAD073/file006 (the name is at most 52 characters long), so that it is linked under this name.
TAPE ?= $(firstword $(wildcard $(TESTDIR)/jadetapes/JAD146/file001) $(TESTDIR)/jadetapes/JAD073/file006)
$(W)/.setup:
	mkdir -p $(W)/jadetapes/JAD073
	for f in aupdat1 aupdat1.b bupdat0 bupdat0.b bupdat1 bupdat1.b; do cp $(DATADIR)/cal/$$f $(W)/; done
	ln -sf $(TAPE) $(W)/jadetapes/JAD073/file006
	touch $@

# --- real data: FPACK -> BOS -> superv -> ze4v -> jzread -> jtjob -> ROOT
$(W)/.data_fptobos: $(W)/.setup
	rm -f $(W)/file006.bos   # fptobos opens its output as a new file
	$(call RUN,$(NAT),fptobos1.card,$(BIN)/fptobos) && touch $@
$(W)/.data_superv: $(W)/.data_fptobos
	$(call RUN,$(SUP),supervfile006.card,$(BIN)/superv) && touch $@
$(W)/.data_ze4v: $(W)/.data_superv
	$(call RUN,$(BE),ze4vfile006.card,$(BIN)/ze4v) && touch $@
$(W)/.data_jzread: $(W)/.data_ze4v
	$(call RUN,$(NAT),jzreadfile006.card,$(BIN)/jzread) && touch $@
$(W)/.data_jtjob: $(W)/.data_jzread
	$(call RUN,$(JTJ),jtjobfile006.card,$(BIN)/jtjob) && touch $@
$(W)/.data_root: $(W)/.data_jtjob
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(H2ROOT) jtjobfile006.hbook jtjobfile006.root > h2root_data.log 2>&1 \
	 && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BLD)/ROOT_analysis_1 jtjobfile006.root jtjobfile006_1.pdf "real data" > ROOT_analysis_data.log 2>&1 && touch $@

# --- Monte Carlo 1: Pythia8 at 34.6 GeV -> HepMC3 -> CPROD -> mcjade -> superv -> ...
$(W)/.mc1_pythia: $(W)/.setup
	cd $(W) && $(BLD)/pythia8_JADE $(TESTDIR)/cards/testPythia1.input testPythia1.output > pythia1.log 2>&1 && touch $@
$(W)/.mc1_cprod: $(W)/.mc1_pythia
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BIN)/convert_hepmc_to_jade -i hepmc3 -o jade testPythia1.output testConvert1.cprod -e Mode=0 > convert1.log 2>&1 && touch $@
$(W)/.mc1_mcjade: $(W)/.mc1_cprod
	$(call RUN,$(NAT),mcjade1.card,$(BIN)/mcjade) && touch $@
$(W)/.mc1_superv: $(W)/.mc1_mcjade
	$(call RUN,$(SUP),superv1.card,$(BIN)/superv) && touch $@
$(W)/.mc1_ze4v: $(W)/.mc1_superv
	$(call RUN,$(BE),ze4v1.card,$(BIN)/ze4v) && touch $@
$(W)/.mc1_jzread: $(W)/.mc1_ze4v
	$(call RUN,$(NAT),jzread1.card,$(BIN)/jzread) && touch $@
$(W)/.mc1_jtjob: $(W)/.mc1_jzread
	$(call RUN,$(JTJ),jtjob1.card,$(BIN)/jtjob) && touch $@
$(W)/.mc1_root: $(W)/.mc1_jtjob
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(H2ROOT) jtjob1.hbook jtjob1.root > h2root1.log 2>&1 \
	 && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BLD)/ROOT_analysis_1 jtjob1.root jtjob11.pdf "Pythia8 hadronic events" > ROOT_analysis1.log 2>&1 && touch $@

# --- Monte Carlo 2: PYTHIA 5.7 at 35 GeV -> mcjade -> superv -> ...
$(W)/.mc2_py57: $(W)/.setup
	rm -f $(W)/py57.cprod $(W)/py57.hist $(W)/mcrnset.dat $(W)/mcrncnt.dat
	$(call RUN,$(NAT),py57.card,$(BIN)/py57) && touch $@
$(W)/.mc2_mcjade: $(W)/.mc2_py57
	$(call RUN,$(NAT),mcjade2.card,$(BIN)/mcjade) && touch $@
$(W)/.mc2_superv: $(W)/.mc2_mcjade
	$(call RUN,$(SUP),superv2.card,$(BIN)/superv) && touch $@
$(W)/.mc2_ze4v: $(W)/.mc2_superv
	$(call RUN,$(BE),ze4v2.card,$(BIN)/ze4v) && touch $@
$(W)/.mc2_jzread: $(W)/.mc2_ze4v
	$(call RUN,$(NAT),jzread2.card,$(BIN)/jzread) && touch $@
$(W)/.mc2_jtjob: $(W)/.mc2_jzread
	$(call RUN,$(JTJ),jtjob2.card,$(BIN)/jtjob) && touch $@
$(W)/.mc2_root: $(W)/.mc2_jtjob
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(H2ROOT) jtjob2.hbook jtjob2.root > h2root2.log 2>&1 \
	 && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BLD)/ROOT_analysis_1 jtjob2.root jtjob12.pdf "PYTHIA 5.7 hadronic events" > ROOT_analysis2.log 2>&1 && touch $@

# --- Monte Carlo 3: Pythia8 at 43.8 GeV
$(W)/.mc3_pythia: $(W)/.setup
	cd $(W) && $(BLD)/pythia8_JADE $(TESTDIR)/cards/testPythia2.input testPythia2.output > pythia3.log 2>&1 && touch $@
$(W)/.mc3_cprod: $(W)/.mc3_pythia
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BIN)/convert_hepmc_to_jade -i hepmc3 -o jade testPythia2.output testConvert2.cprod -e Mode=0 > convert3.log 2>&1 && touch $@
$(W)/.mc3_mcjade: $(W)/.mc3_cprod
	$(call RUN,$(NAT),mcjade3.card,$(BIN)/mcjade) && touch $@
$(W)/.mc3_superv: $(W)/.mc3_mcjade
	$(call RUN,$(SUP),superv3.card,$(BIN)/superv) && touch $@
$(W)/.mc3_ze4v: $(W)/.mc3_superv
	$(call RUN,$(BE),ze4v3.card,$(BIN)/ze4v) && touch $@
$(W)/.mc3_jzread: $(W)/.mc3_ze4v
	$(call RUN,$(NAT),jzread3.card,$(BIN)/jzread) && touch $@
$(W)/.mc3_jtjob: $(W)/.mc3_jzread
	$(call RUN,$(JTJ),jtjob3.card,$(BIN)/jtjob) && touch $@
$(W)/.mc3_root: $(W)/.mc3_jtjob
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(H2ROOT) jtjob3.hbook jtjob3.root > h2root3.log 2>&1 \
	 && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BLD)/ROOT_analysis_1 jtjob3.root jtjob13.pdf "Pythia8 hadronic events" > ROOT_analysis3.log 2>&1 && touch $@

# --- Monte Carlo 4: Pythia8 e+ e- -> mu+ mu- at 34.6 GeV (the momentum of the muons is the beam energy)
$(W)/.mc4_pythia: $(W)/.setup
	cd $(W) && $(BLD)/pythia8_JADE $(TESTDIR)/cards/testPythia3.input testPythia3.output > pythia4.log 2>&1 && touch $@
$(W)/.mc4_cprod: $(W)/.mc4_pythia
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BIN)/convert_hepmc_to_jade -i hepmc3 -o jade testPythia3.output testConvert4.cprod -e Mode=0 > convert4.log 2>&1 && touch $@
$(W)/.mc4_mcjade: $(W)/.mc4_cprod
	$(call RUN,$(NAT),mcjade4.card,$(BIN)/mcjade) && touch $@
$(W)/.mc4_superv: $(W)/.mc4_mcjade
	$(call RUN,$(SUP),superv4.card,$(BIN)/superv) && touch $@

# --- Monte Carlo 5: dummy generator e+ e- -> pi+ pi- at 34.6 GeV
$(W)/.mc5_gen: $(W)/.setup
	cd $(W) && $(BLD)/dummy_gen testDummy5.output 1000 34.6 211 > dummy5.log 2>&1 && touch $@
$(W)/.mc5_cprod: $(W)/.mc5_gen
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BIN)/convert_hepmc_to_jade -i hepmc3 -o jade testDummy5.output testConvert5.cprod -e Mode=0 > convert5.log 2>&1 && touch $@
$(W)/.mc5_mcjade: $(W)/.mc5_cprod
	$(call RUN,$(NAT),mcjade5.card,$(BIN)/mcjade) && touch $@
$(W)/.mc5_superv: $(W)/.mc5_mcjade
	$(call RUN,$(SUP),superv5.card,$(BIN)/superv) && touch $@

# --- Monte Carlo 6: dummy generator e+ e- -> K0S K0S at 14 GeV (the K0S decays to pi+ pi- with the lifetime)
$(W)/.mc6_gen: $(W)/.setup
	cd $(W) && $(BLD)/dummy_gen testDummy6.output 1000 14 310 > dummy6.log 2>&1 && touch $@
$(W)/.mc6_cprod: $(W)/.mc6_gen
	cd $(W) && env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH $(BIN)/convert_hepmc_to_jade -i hepmc3 -o jade testDummy6.output testConvert6.cprod -e Mode=0 > convert6.log 2>&1 && touch $@
$(W)/.mc6_mcjade: $(W)/.mc6_cprod
	$(call RUN,$(NAT),mcjade6.card,$(BIN)/mcjade) && touch $@
$(W)/.mc6_superv: $(W)/.mc6_mcjade
	$(call RUN,$(SUP),superv6.card,$(BIN)/superv) && touch $@

# --- K0S -> pi+ pi- analysis (test/src/k0s.f) of the BOS files of superv
# (k0s stops with an error if no pair is combined, e.g. for the pion pairs, which is no failure here)
$(W)/.k0s: $(W)/.data_superv $(W)/.mc1_superv $(W)/.mc3_superv $(W)/.mc4_superv $(W)/.mc5_superv $(W)/.mc6_superv
	cd $(W) && for s in "data_1 supervfile006" "mc_1 superv1" "mc_3 superv3" "mc_4 superv4" "mc_5 superv5" "mc_6 superv6"; do set -- $$s; \
	  env $(K0SE) $(BLD)/k0s $(W)/$$2.bos $(W)/k0s_$$1.txt $(W)/k0s_$$1_dist.txt $(TESTDIR)/cards/k0s.card > k0s_$$1.log 2>&1 || true; done && touch $@

# --- event display (jadez, needs a virtual X display): the scripts of the tests make the pictures
# JADE.nnn.ps in $(W)/rphidisplay and $(W)/mcdisplay
$(W)/.display: $(W)/.mc1_superv
	env 'GFORTRAN_CONVERT_UNIT=big_endian;native:2' bash $(TESTDIR)/jadez_rphi_test.sh $(BIN)/jadez $(DATADIR)/F22BOW.HELP.DATASET $(W) > $(W)/jadez_rphi.log 2>&1
	env 'GFORTRAN_CONVERT_UNIT=big_endian;native:2' bash $(TESTDIR)/jadez_mc_test.sh $(BIN)/jadez $(DATADIR)/F22BOW.HELP.DATASET $(W) > $(W)/jadez_mc.log 2>&1
	touch $@

# --- the figures
$(FIG)/display_ref%.pdf: ../eventdisplayreferences/%.jpg
	mkdir -p $(FIG)
	cp $< $(FIG)/display_ref$*.jpg
	convert $< $@

# HIGZ PostScript -> cropped PDF
$(FIG)/display_rphi.pdf: $(W)/.display      # JADEZ_RPHI_1 session: RB1 and DEDX -4, like display_ref2
	ps2eps -f -l -q < $(W)/rphidisplay/JADE.001.ps > $(FIG)/display_rphi.tmp.eps
	gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite -dEPSCrop -sOutputFile=$@ $(FIG)/display_rphi.tmp.eps
	rm -f $(FIG)/display_rphi.tmp.eps
$(FIG)/display_mc_rc1.pdf: $(W)/.display    # JADEZ_MC_1 session: RC1, the hardware with the muon chambers
	ps2eps -f -l -q < $(W)/mcdisplay/JADE.002.ps > $(FIG)/display_mc_rc1.tmp.eps
	gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite -dEPSCrop -sOutputFile=$@ $(FIG)/display_mc_rc1.tmp.eps
	rm -f $(FIG)/display_mc_rc1.tmp.eps

# thrust: ROOT_analysis_1 writes <name>.pdf, so the files are called *.pdf.pdf
$(FIG)/thrust_data.pdf: $(W)/.data_root
	cp $(W)/jtjobfile006_1.pdf.pdf $@
$(FIG)/thrust_mc1.pdf: $(W)/.mc1_root
	cp $(W)/jtjob11.pdf.pdf $@
$(FIG)/thrust_mc2.pdf: $(W)/.mc2_root
	cp $(W)/jtjob12.pdf.pdf $@
$(FIG)/thrust_mc3.pdf: $(W)/.mc3_root
	cp $(W)/jtjob13.pdf.pdf $@

# sums of the energies of the tracks and of the clusters of an event (also written by ROOT_analysis_1)
$(FIG)/energy_data.pdf: $(W)/.data_root
	cp $(W)/jtjobfile006_1.pdf_energy.pdf $@
$(FIG)/energy_mc1.pdf: $(W)/.mc1_root
	cp $(W)/jtjob11.pdf_energy.pdf $@
$(FIG)/energy_mc2.pdf: $(W)/.mc2_root
	cp $(W)/jtjob12.pdf_energy.pdf $@
$(FIG)/energy_mc3.pdf: $(W)/.mc3_root
	cp $(W)/jtjob13.pdf_energy.pdf $@

# K0S: the ROOT macro k0s_plots.C reads the k0s_*.txt files of $(W)
$(FIG)/k0s_mass.pdf $(FIG)/k0s_cuts.pdf $(FIG)/k0s_event.pdf $(FIG)/k0s_energy.pdf \
  $(FIG)/k0s_mass_lept.pdf $(FIG)/k0s_cuts_lept.pdf $(FIG)/k0s_event_lept.pdf $(FIG)/k0s_energy_lept.pdf &: $(W)/.k0s k0s_plots.C
	env LD_LIBRARY_PATH=$(ROOTLIB):$$LD_LIBRARY_PATH root -l -b -q "k0s_plots.C(\"$(W)\",\"$(FIG)\")" > /dev/null 2>&1

clean :
	rm -f $(latexfile)-papers.tex
	rm -f *.aux
	rm -f *.lof
	rm -f *.out
	rm -f *.lot
	rm -f *.bbl
	rm -f *.l4l
	rm -f *.log
	rm -f *.blg
	rm -f *.ilg
	rm -f *.aux
	rm -f $(latexfile)*.dvi
	rm -f $(latexfile)*.ps
	rm -f $(latexfile)*.pdf
	rm -f *.toc
	rm -f *.idx
	rm -f *.ind
	rm -f *.nav
	rm -f *.snm
