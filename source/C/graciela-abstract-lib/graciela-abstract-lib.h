//
//  Header.h
//  graciela-abstract-lib
//
//  Created by Carlos Spaggiari Roa on 8/20/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#ifndef Header_h
#define Header_h

#ifdef __cplusplus
#include <cstdio>
#include <cstdlib>
#include <set>
#include <map>
#include <vector>
#include <algorithm>
#else
#include <stdlib.h>
#endif


typedef int64_t t;
#ifdef __cplusplus
using namespace std;
namespace glib {
    
    typedef enum{
        SET      = 0,
        MULTISET = 1,
        FUNCTION = 2,
        RELATION = 3,
        SEQUENCE = 4
    } type;

    typedef map<t,t>           Function;
    typedef pair<t,t>          Tuple;
    typedef set<t>             Set;
    typedef set<Tuple>         Relation;
    typedef multiset<t>        Multiset;
    typedef vector<t>          Sequence;
    typedef pair<int8_t*,type> TCTuple;
    typedef vector<TCTuple>    TrashCollector;
}

extern "C" {
#endif
    
    typedef struct Iterator {
        t       data;
        int8_t* it;
        int8_t* type;
    }Iterator;
    
    Iterator *first(int8_t *ptr);
    Iterator *next(Iterator* i);
    
    /* Set */
    int8_t*  newSet();

    int      equalSet(int8_t* ptr1, int8_t* ptr2);
    void     insertSet(int8_t* ptr, t x);
    int      sizeSet(int8_t* ptr);
    int      isElemSet(int8_t* ptr, t x);
    int8_t*  unionSet(int8_t* ptr1, int8_t* ptr2);
    int8_t*  intersectSet(int8_t* ptr1, int8_t* ptr2);
    int8_t*  differenceSet(int8_t* ptr1, int8_t* ptr2);
    void     freeSet(int8_t* ptr);
    
    /* Multiset */
    int8_t* newMultiset();
    int     equalMultiset(int8_t* ptr1, int8_t* ptr2);
    void    insertMultiset(int8_t* ptr, t x);
    int     isElemMultiset(int8_t* ptr, t x);
    int     sizeMultiset(int8_t* ptr);
    int     countMultiset(int8_t* ptr, t x);
    int8_t* unionMultiset(int8_t* ptr1, int8_t* ptr2);
    int8_t* intersectMultiset(int8_t* ptr1, int8_t* ptr2);
    int8_t* differenceMultiset(int8_t* ptr1, int8_t* ptr2);
    void    freeMultiset(int8_t* ptr);
    
    /* Function */
    int8_t* newFunction();
    int     equalFunction(int8_t* ptr1, int8_t* ptr2);
    void    insertFunction(int8_t* ptr, t key, t value);
    int     sizeFunction(int8_t *ptr);
    int     isElemFunction(int8_t* ptr, t key, t value);
    int8_t* domFunction(int8_t* ptr);
    int     pairFunction(int8_t* ptr, t k);
    int8_t* compositionFunction(int8_t *ptr1, int8_t *ptr2);
    void    freeFunction(int8_t* ptr);
    
    /* Relation */
    int8_t* newRelation();
    int     equalRelation(int8_t* ptr1, int8_t* ptr2);
    void    insertRelation(int8_t* ptr, t key, t value);
    int     sizeRelation(int8_t *ptr);
    int     isElemRelation(int8_t* ptr, t key, t value);
    int8_t* domRelation(int8_t* ptr);
    int8_t* pairRelation(int8_t* ptr, t key);
    int8_t* compositionRelation(int8_t *ptr1, int8_t *ptr2);
    void    freeRelation(int8_t* ptr);
    
    /* Sequence */
    int8_t* newSequence();
    int     equalSequence(int8_t* ptr1, int8_t* ptr2);
    int     isElemSequence(int8_t* ptr, t x);
    void    insertSequence(int8_t* ptr, t x);
    int     sizeSequence(int8_t* ptr);
    void    freeSequence(int8_t* ptr);
    
    /* TrashCollector 
     *  Every pointer to a type created (set, multiset, ...) is
     *  store inside a vector of pointer, to be freed when freeGarbage()
     *  is called.
     */
    void    newTrashCollector();
    void    freeGarbage();
    void    freeTrashCollector();
#ifdef __cplusplus
}
#endif
#endif /* Header_h */
