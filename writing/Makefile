all: teamcloud.pdf 
 
teamcloud.pdf: teamcloud.Rnw 
	Rscript -e 'library(knitr);Sweave2knitr("teamcloud.Rnw")'
	Rscript -e 'library(knitr);knit("teamcloud-knitr.Rnw")'
	pdflatex teamcloud-knitr.tex
#	bibtex teamcloud-knitr
	pdflatex teamcloud-knitr.tex
	pdflatex teamcloud-knitr.tex
	cp teamcloud-knitr.pdf teamcloud.pdf
#	dvipdf teamcloud-knitr.dvi teamcloud.pdf

clean:
	rm teamcloud.pdf teamcloud-knitr.* 
