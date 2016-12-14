//
//  main.c
//  libgraciela-abstract
//
//  Created by Carlos Spaggiari Roa on 8/20/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include "libgraciela-abstract.h"



int main() {
    _initTrashCollector();
    _openScope();

//  printf("hola\n");
    int8_t *a = _newSet();

//    int8_t *func = _newFunction();
//    _insertFunction(func, 1, 2);
//    _insertFunction(func, 1, 12);
//    printf("(1,2)@func  == %s\n",_isElemFunction(func, 1,2)?"True":"False");
//    printf("(1,12)@func == %s\n",_isElemFunction(func, 1,12)?"True":"False");
//
//    _openScope();
//    int8_t *rel = _newRelation();
//    _insertRelation(rel, 1, 2);
//    _insertRelation(rel, 10, 12);
//    printf("(1,2)@rel  == %s\n",_isElemRelation(rel, 1,2)?"True":"False");
//    printf("(1,12)@rel == %s\n",_isElemRelation(rel, 1,12)?"True":"False");
//
//
//    _closeScope();
//
    _insertSet(a,1);
    _insertSet(a,2);
    _insertSet(a,3);
    _insertSet(a,5);
    _insertSet(a,6);
    _insertSet(a,7);
    struct Iterator *it = _firstSet(a);
    while (it != NULL){
      printf("%lli\n",it->data);
      it = _nextSet(it);
    }
//
//    printf("%s\n",_isElemSet(a, 1)?"True":"False");
//
//    int8_t *mul = _newMultiset();
//    _insertMultiset(mul, 1);
//    _insertMultiset(mul, 2);
//    _insertMultiset(mul, 3);
//    _insertMultiset(mul, 4);
//    _insertMultiset(mul, 5);
//    printf("%d <- \n",_isElemMultiset(mul, 1));
  int8_t c[5] = {'a','b','c','d','e'};
  int8_t b[5];
  int8_t* c_ = (int8_t*)c, *b_ = (int8_t*)b;
  int *lo = NULL;

  int j = 0;
  for (int i = 0;  i < 5*4 ; ++i) {
    printf(".. %p == %p  --> %d \n",&c[i], (c_+j), c[i]);
    *(b_+j) = *(c_+j);
    printf("++ %p == %p  --> %d =? %d \n\n",&b[i], (b_+j), *(b_+j),b[i]);
    ++j;
  }
//  lo = (int*)malloc(4);
  for (int i = 0;  i < 5 ; ++i) {
    printf(">> %d\n",10);
  }
<<<<<<< HEAD:source/C/libgraciela-abstract/main.c
  printf("%d\n", *lo);
=======

>>>>>>> post:source/C/libgraciela-abstract/main.c
    _freeTrashCollector();
    return 0;
}
