HS_FILES = $(shell find src/Haskell/ -type f -name '*.hs' -o -name '*.hs-boot')

all : graciela libgraciela.so libgraciela-abstract.so
.PHONY : all

graciela: $(HS_FILES)
	@ghc -outputdir .build -isrc/Haskell src/Haskell/Main.hs -o graciela > /dev/null
	@strip graciela

libgraciela.so: src/C/libgraciela.c
	@clang-3.5          -shared -fPIC \
	-o libgraciela.so \
	src/C/libgraciela.c
	@strip libgraciela.so

libgraciela-abstract.so: src/C/libgraciela-abstract/libgraciela-abstract.cpp src/C/libgraciela-abstract/libgraciela-abstract.h
	@clang-3.5 -lstdc++ -shared  -fPIC \
	-o libgraciela-abstract.so \
	src/C/libgraciela-abstract/libgraciela-abstract.cpp
	@strip libgraciela-abstract.so

clean: cleanc cleanhs

cleanc:
	rm -f libgraciela.so libgraciela-abstract.so

cleanhs:
	rm -rf .build graciela

install:
	install graciela $(DESTDIR)/usr/bin
	install src/Bash/rungraciela $(DESTDIR)/usr/bin
	install -m 644 libgraciela.so libgraciela-abstract.so $(DESTDIR)/usr/lib/graciela
