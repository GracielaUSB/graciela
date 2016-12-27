HS_FILES = $(shell find src/Haskell/ -type f -name '*.{hs,hs-boot}')

all : graciela libgraciela.so libgraciela-abstract.so
.PHONY : all

graciela: $(HS_FILES)
	ghc -outputdir .build -isrc/Haskell src/Haskell/Main.hs -o graciela

libgraciela.so: src/C/libgraciela.c
	clang-3.5 -fPIC --shared -o libgraciela.so src/C/libgraciela.c

libgraciela-abstract.so: src/C/libgraciela-abstract/libgraciela-abstract.cpp src/C/libgraciela-abstract/libgraciela-abstract.h
	clang-3.5 -lstdc++ -fPIC --shared -o libgraciela-abstract.so src/C/libgraciela-abstract/libgraciela-abstract.cpp

clean:
	rm -rf .build
	rm -f graciela libgraciela.so libgraciela-abstract.so

install:
	cp graciela /usr/bin
	cp src/Bash/rungraciela /usr/bin
	cp libgraciela.so libgraciela-abstract.so /usr/lib
