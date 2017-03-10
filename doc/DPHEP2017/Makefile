file=DPHEP2017_Andrii_Verbytskyi
all:  figures
		latex $(file).tex
		latex $(file).tex
		latex $(file).tex
		dvips $(file).dvi
		ps2pdf $(file).ps		
#		pdftk A=$(file).pdf B=../$(file).pdf  cat A B output  $(file).pdf
clean:
		rm *log *aux
		
spell: 
		for a in $$(ls -1 $(latexfile)*.tex); do\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		echo $a;\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		aspell   -t --dont-tex-check-comments -d en_GB -c $$a ;\
		done
figures:
	convert  pngjpggif/JADE-ID-records.jpg eps/JADE-ID-records.eps
	convert pngjpggif/JADEDet.jpeg eps/JADEDet.eps
	convert pngjpggif/JADE_Logbook.jpg eps/JADE_Logbook.eps
	convert pngjpggif/USB.jpg eps/USB.eps
	convert pngjpggif/MPCDF.png eps/MPCDF.eps
