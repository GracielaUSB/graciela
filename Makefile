HS_FILES = $(shell find src/Haskell/ -type f -name '*.hs' -o -name '*.hs-boot')

all : graciela libgraciela.so
.PHONY : all

graciela: $(HS_FILES)
	@ghc -outputdir .build -isrc/Haskell src/Haskell/Main.hs -o graciela > /dev/null
	@strip graciela

libgraciela.so: src/C/libgraciela.c src/C/libgraciela-abstract/libgraciela-abstract.cpp src/C/libgraciela-abstract/libgraciela-abstract.h
	@clang-3.5 -lm -lstdc++ -fPIC -shared \
 	src/C/libgraciela-abstract/libgraciela-abstract.cpp \
	src/C/libgraciela.c \
	-o libgraciela.so
	@strip libgraciela.so

# libgraciela-abstract.so: src/C/libgraciela-abstract/libgraciela-abstract.cpp src/C/libgraciela-abstract/libgraciela-abstract.h
# 	@clang-3.5 -lstdc++ -shared  -fPIC \
# 	-o libgraciela-abstract.so \
# 	src/C/libgraciela-abstract/libgraciela-abstract.cpp
# 	@strip libgraciela-abstract.so

test: graciela libgraciela.so testset/Test.hs
	@ghc -outputdir .build -isrc/Haskell:testset testset/Test.hs -main-is Test -o gratest > /dev/null
	./gratest

clean: cleanc cleanhs cleantest

cleanc:
	rm -f libgraciela.so

cleanhs:
	rm -rf .build graciela

cleantest:
	rm -rf gratest

install:
	install graciela $(DESTDIR)/usr/bin
	install src/Bash/rungraciela $(DESTDIR)/usr/bin
	install -m 644 libgraciela.so $(DESTDIR)/usr/lib
