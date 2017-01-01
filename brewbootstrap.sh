cd src/C
clang-3.5 -lstdc++ -fPIC -shared ./libgraciela-abstract.cpp -o ./libgraciela-abstract.so
clang-3.5 -fPIC -shared ./libgraciela.c ./libgraciela-abstract.so -o ./libgraciela.so
mv ./libgraciela-abstract.so ../../
mv ./libgraciela.so ../../

cd ../Haskell
cabal update
cabal install llvm-general -fshared-llvm
cabal install --disable-documentation
mv ./dist/build/graciela/graciela ../../graciela