//
//  main.cpp
//  graciela-abstract-lib
//
//  Created by Carlos Spaggiari Roa on 8/20/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include "graciela-abstract-lib.h"



int main() {
//    initTC();
//    openScope();

  printf("hola\n");
    int8_t *a = newSet();

    int8_t *func = newFunction();
    insertFunction(func, 1, 2);
    insertFunction(func, 1, 12);
    printf("(1,2)@func  == %s\n",isElemFunction(func, 1,2)?"True":"False");
    printf("(1,12)@func == %s\n",isElemFunction(func, 1,12)?"True":"False");
    
    openScope();
    int8_t *rel = newRelation();
    insertRelation(rel, 1, 2);
    insertRelation(rel, 10, 12);
    printf("(1,2)@rel  == %s\n",isElemRelation(rel, 1,2)?"True":"False");
    printf("(1,12)@rel == %s\n",isElemRelation(rel, 1,12)?"True":"False");
    
    struct Iterator *it = first(rel);
    while (it != NULL){
        printf("%lli\n",it->data);
        it = next(it);
    }
    closeScope();
    
    insertSet(a,1);
    insertSet(a,2);
    insertSet(a,3);
        insertSet(a,5);
        insertSet(a,6);
        insertSet(a,7);
    printf("%s\n",isElemSet(a, 1)?"True":"False");
    
    int8_t *mul = newMultiset();
    insertMultiset(mul, 1);
    insertMultiset(mul, 2);
    insertMultiset(mul, 3);
    insertMultiset(mul, 4);
    insertMultiset(mul, 5);
    printf("%d <- \n",isElemMultiset(mul, 1));

    
    freeTrashCollector();
    return 0;
}
