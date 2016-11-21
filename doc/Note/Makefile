latexfile=JADE2016
newname?=FFC
STYLE?=draft

LATEX=latex -halt-on-error -shell-escape
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

$(latexfile)-$(STYLE).dvi :  $(figures) $(constanttables) $(tables) $(latexfile).bst\
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

$(latexfile)-$(STYLE).pdf : $(latexfile)-$(STYLE).ps
	ps2pdf -dUseCIEColor -dPDFSETTINGS=/printer -sPAPERSIZE=a4  $(latexfile)-$(STYLE).ps $(latexfile)-$(STYLE).pdf

ps : $(latexfile)-$(STYLE).ps 

pdf : $(latexfile)-$(STYLE).pdf
$(latexfile)-$(STYLE).ps : $(latexfile)-$(STYLE).dvi
	dvips -Ppdf -t a4 $(latexfile)-$(STYLE)


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
