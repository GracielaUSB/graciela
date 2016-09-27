all: informe clean

informe: informe1
	pdflatex 0-Informe.tex

informe1: informe2
	pdflatex 0-Informe.tex

informe2: informe3
	pdflatex 0-Informe.tex

informe3: 
	pdflatex 0-Informe.tex

cleanall: clean
	rm -f *.pdf

clean:
	rm -f *.aux *.toc *.out *.log *.lof *.brf *.lot
