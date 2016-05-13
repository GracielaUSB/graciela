#!/bin/bash

compile(){
    if $GACELA $1
    then if llc-3.5 -filetype=obj "${1/%.gcl/.bc}"
        then if gcc "${1/%.gcl/.o}" $GCLAUX -o "${1/%.gcl/}"
            then
                echo "success!"
                rm "${1/%.gcl/.o}"
                rm "${1/%.gcl/.bc}"
            else echo "failed at step 'gcc'"
            fi
        else echo "failed at step 'llc'"
        fi
    else echo "failed at step 'gacela'"
    fi
}

GACELA=/home/mal/git/gacela/source/dist/build/gacela/gacela
GCLAUX=/lib/x86_64-linux-gnu/auxiliarFunctions.so

compile $1
