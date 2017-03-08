file=DPHEP2017_Andrii_Verbytskyi
all: 
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
figurese:
	convert -scale 20% pngjpggif/DESY-Logo-cyan-RGB_ger.jpg  eps/DESY-Logo-cyan-RGB_ger.eps
	convert  pngjpggif/iso-image.png  eps/iso-image.eps
	convert  pngjpggif/Screenshot_2.png eps/Screenshot_2.eps
	convert  pngjpggif/sw3.png eps/sw3.eps
	convert  pngjpggif/Hummel3ed.png eps/Hummel3ed.eps	
	convert  -scale 20% pngjpggif/bigstock-Scales-Of-Justice-1182062.jpg eps/law.eps	
	convert  -scale 30% pngjpggif/Schw.png eps/Schw.eps
	convert   pngjpggif/oeteker.jpg  eps//oeteker.eps
	convert   pngjpggif/bg.png  eps/bg.eps
	convert   pngjpggif/desy-1.jpg  eps/desy-1.eps
	convert   pngjpggif/question-mark-2-xxl.png eps/question-mark-2-xxl.eps
	convert   pngjpggif/yellow-arrow-orange-hi.png	eps/yellow-arrow-orange-hi.eps
	convert   pngjpggif/egypt-pyramid.jpg eps/egypt-pyramid.eps 
	convert   pngjpggif/zeus-detektor.gif eps/zeus-detektor.eps
	convert   pngjpggif/rootlogo.png eps/rootlogo.eps
	convert   pngjpggif/sqlite370_banner.gif eps/sqlite370_banner.eps
	convert   pngjpggif/recipe_image_1045.jpg eps/recipe_image_1045.eps
	convert   pngjpggif/hermes.jpg eps/hermesdetector.eps
	convert   pngjpggif/H1.png eps/h1detector.eps
