all: unlikely.pdf 
 
unlikely.pdf: unlikely.Rnw 
	Rscript -e 'library(knitr);Sweave2knitr("unlikely.Rnw")'
	Rscript -e 'library(knitr);knit("unlikely-knitr.Rnw")'
	pdflatex unlikely-knitr.tex
#	bibtex unlikely-knitr
	pdflatex unlikely-knitr.tex
	pdflatex unlikely-knitr.tex
	cp unlikely-knitr.pdf unlikely.pdf
#	dvipdf unlikely-knitr.dvi unlikely.pdf

clean:
	rm unlikely.pdf unlikely-knitr.* 
