HS_FILES = $(shell find src/Haskell/ -type f -name '*.hs' -o -name '*.hs-boot')

# ifeq ($(TARGET), i386)
#  C_FLAGS=-m32
#  HS_FLAGS=-opta-m32 -optc-m32 -optl-m32
# else
#  C_FLAGS=
#  HS_FLAGS=
# endif

all: lib exe

exe: graciela

lib: libgraciela.so

graciela: $(HS_FILES)
	@stack install
	@strip graciela

libgraciela.so: src/C/libgraciela.c src/C/libgraciela-abstract/libgraciela-abstract.cpp src/C/libgraciela-abstract/libgraciela-abstract.h
	@clang-3.5 $(C_FLAGS) -lm -lstdc++ -fPIC -shared \
 	src/C/libgraciela-abstract/libgraciela-abstract.cpp \
	src/C/libgraciela.c \
	-o libgraciela.so
	@strip libgraciela.so

test: graciela libgraciela.so testset/Test.hs
	@stack test

clean: cleanc cleanhs cleantest

cleanc:
	@rm -f libgraciela.so

cleanhs:
	@stack clean
	@rm -rf .build graciela

cleantest:
	@rm -rf gratest

install:
	install graciela $(DESTDIR)/usr/bin
	install src/Bash/rungraciela $(DESTDIR)/usr/bin
	install -m 644 libgraciela.so $(DESTDIR)/usr/lib
