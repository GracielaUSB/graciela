//
//  main.cpp
//  graciela-abstract-lib
//
//  Created by Carlos Spaggiari Roa on 8/20/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>

/* Set */
int8_t* newSet();
int     equalSet(int8_t* ptr1, int8_t* ptr2);
void    insertSet(int8_t* ptr, int x);
int     sizeSet(int8_t* ptr);
int     isElemSet(int8_t* ptr, int x);
int8_t* unionSet(int8_t* ptr1, int8_t* ptr2);
int8_t* intersectSet(int8_t* ptr1, int8_t* ptr2);
int8_t* differenceSet(int8_t* ptr1, int8_t* ptr2);
void    freeSet(int8_t* ptr);

/* Multiset */
int8_t* newMultiset();
int     equalMultiset(int8_t* ptr1, int8_t* ptr2);
void    insertMultiset(int8_t* ptr, int x);
int     isElemMultiset(int8_t* ptr, int x);
int     sizeMultiset(int8_t* ptr);
int     countMultiset(int8_t* ptr, int x);
int8_t* unionMultiset(int8_t* ptr1, int8_t* ptr2);
int8_t* intersectMultiset(int8_t* ptr1, int8_t* ptr2);
int8_t* differenceMultiset(int8_t* ptr1, int8_t* ptr2);
void    freeMultiset(int8_t* ptr);

/* Function */
int8_t* newFunction();
int     equalFunction(int8_t* ptr1, int8_t* ptr2);
void    insertFunction(int8_t* ptr, int key, int value);
int     sizeFunction(int8_t *ptr);
int     isElemFunction(int8_t* ptr, int key, int value);
int8_t* domFunction(int8_t* ptr);
int     pairFunction(int8_t* ptr, int k);
int8_t* compositionFunction(int8_t *ptr1, int8_t *ptr2);
void    freeFunction(int8_t* ptr);

/* Relation */
int8_t* newRelation();
int     equalRelation(int8_t* ptr1, int8_t* ptr2);
void    insertRelation(int8_t* ptr, int key, int value);
int     sizeRelation(int8_t *ptr);
int     isElemRelation(int8_t* ptr, int key, int value);
int8_t* domRelation(int8_t* ptr);
int8_t* pairRelation(int8_t* ptr, int key);
int8_t* compositionRelation(int8_t *ptr1, int8_t *ptr2);
void    freeRelation(int8_t* ptr);

/* Sequence */
int8_t* newSequence();
int     equalSequence(int8_t* ptr1, int8_t* ptr2);
int     isElemSequence(int8_t* ptr, int x);
void    insertSequence(int8_t* ptr, int x);
int     sizeSequence(int8_t* ptr);
void    freeSequence(int8_t* ptr);

/* TrashCollector */
void    newTrashCollector();
void    freeGarbage();
void    freeTrashCollector();

int main() {
    newTrashCollector();
    int8_t *a = newSet();
    
    int8_t *func = newFunction();
    insertFunction(func, 1, 2);
    insertFunction(func, 1, 12);
    printf("(1,2)@func  == %s\n",isElemFunction(func, 1,2)?"True":"False");
    printf("(1,12)@func == %s\n",isElemFunction(func, 1,12)?"True":"False");
    
    int8_t *rel = newRelation();
    insertRelation(rel, 1, 2);
    insertRelation(rel, 1, 12);
    printf("(1,2)@rel  == %s\n",isElemRelation(rel, 1,2)?"True":"False");
    printf("(1,12)@rel == %s\n",isElemRelation(rel, 1,12)?"True":"False");
    
    insertSet(a,1);
    insertSet(a,1);
    insertSet(a,1);
    printf("%s\n",isElemSet(a, 1)?"True":"False");
    
    int8_t *mul = newMultiset();
    insertMultiset(mul, 1);
    insertMultiset(mul, 1);
    insertMultiset(mul, 1);
    insertMultiset(mul, 1);
    insertMultiset(mul, 1);
    printf("%d <- \n",isElemMultiset(mul, 1));
    freeGarbage();
    freeTrashCollector();
    
    return 0;
}
