all: Main.hs
	 ghc -o Main --make Main.hs

shared: auxiliarFunctions.c
	 gcc -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so 

clean:
	$(RM) Main main *.hs~ *.o *.hi prueba.bc prueba prueba.s
