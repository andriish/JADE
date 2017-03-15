#file=DPHEP2017_JADE_Andrii_Verbytskyi
file=DPHEP2017_OPAL_Andrii_Verbytskyi
all: all1 all2
all1:  figures
		latex DPHEP2017_JADE_Andrii_Verbytskyi.tex
		latex DPHEP2017_JADE_Andrii_Verbytskyi.tex
		latex DPHEP2017_JADE_Andrii_Verbytskyi.tex
		dvips DPHEP2017_JADE_Andrii_Verbytskyi.dvi
		ps2pdf DPHEP2017_JADE_Andrii_Verbytskyi.ps		


all2:  figures
		latex DPHEP2017_OPAL_Andrii_Verbytskyi.tex
		latex DPHEP2017_OPAL_Andrii_Verbytskyi.tex
		latex DPHEP2017_OPAL_Andrii_Verbytskyi.tex
		dvips DPHEP2017_OPAL_Andrii_Verbytskyi.dvi
		ps2pdf DPHEP2017_OPAL_Andrii_Verbytskyi.ps		
#		pdftk A=$(file).pdf B=../$(file).pdf  cat A B output  $(file).pdf
clean:
		rm *log *aux

dict.rws: dict.txt
	aspell --lang=en create master ./dict.rws < ./dict.txt

		
spell: dict.rws 
		for a in $$(ls -1 $(latexfile)*.tex); do\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		echo $a;\
		echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" ;\
		aspell   --add-extra-dicts=./dict.rws -t --dont-tex-check-comments -d en_GB -c $$a ;\
		done
figures:
	convert  pngjpggif/JADE-ID-records.jpg eps/JADE-ID-records.eps
	convert pngjpggif/JADEDet.jpeg eps/JADEDet.eps
	convert pngjpggif/JADE_Logbook.jpg eps/JADE_Logbook.eps
	convert pngjpggif/USB.jpg eps/USB.eps
	convert pngjpggif/MPCDF.png eps/MPCDF.eps
	convert pngjpggif/compilation.png eps/compilation.eps
	convert pngjpggif/compilation2.png eps/compilation2.eps
	convert pngjpggif/jadezroot.png eps/jadezroot.eps
	convert pngjpggif/start.png eps/start.eps
	convert pngjpggif/thrust.png eps/thrust.eps
	convert pngjpggif/sherpa.png eps/sherpa.eps
