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
#include <stack>
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
        SEQUENCE = 4,
        SETPAIR  = 5
    } type;

    typedef map<t,t>           Function;
    typedef pair<t,t>          Tuple;
    typedef set<t>             Set;
    typedef set<Tuple>         SetPair;
    typedef set<Tuple>         Relation;
    typedef multiset<t>        Multiset;
    typedef vector<t>          Sequence;
    typedef pair<int8_t*,type> TCTuple;
    typedef vector<TCTuple>    TrashCollector;
    typedef stack<TrashCollector> stack;
}

extern "C" {
#endif
    
    typedef struct gtuple {
        int64_t a;
        int64_t b;
    } gtuple;
    
    typedef struct Iterator {
        t       data;
        int8_t* it;
        int8_t* type;
    }Iterator;
    
    Iterator *_first(int8_t *ptr);
    Iterator *_next(Iterator* i);
    
    /* Set */
    int8_t*  _newSet();
    int      _equalSet(int8_t* ptr1, int8_t* ptr2);
    void     _insertSet(int8_t* ptr, t x);
    int      _sizeSet(int8_t* ptr);
    int      _isElemSet(int8_t* ptr, t x);
    int8_t*  _unionSet(int8_t* ptr1, int8_t* ptr2);
    int8_t*  _intersectSet(int8_t* ptr1, int8_t* ptr2);
    int8_t*  _differenceSet(int8_t* ptr1, int8_t* ptr2);
    int      _includesSet(int8_t* ptr1, int8_t* ptr2);
    void     _freeSet(int8_t* ptr);
    /* SetPair */
    
    int8_t* _newSetPair();
    int     _equalSetPair(int8_t *ptr1, int8_t* ptr2);
    void    _insertSetPair(int8_t *ptr, gtuple x);
    int     _sizeSetPair(int8_t *ptr);
    int     _isElemSetPair(int8_t *ptr, gtuple x);
    int8_t* _unionSetPair(int8_t *ptr1, int8_t * ptr2);
    int8_t* _intersectSetPair(int8_t *ptr1, int8_t * ptr2);
    int8_t* _differenceSetPair(int8_t *ptr1, int8_t * ptr2);
    int     _includesSetPair(int8_t* ptr1, int8_t* ptr2);
    void    _freeSetPair(int8_t* ptr);

    /* Multiset */
    int8_t* _newMultiset();
    int     _equalMultiset(int8_t* ptr1, int8_t* ptr2);
    void    _insertMultiset(int8_t* ptr, t x);
    int     _isElemMultiset(int8_t* ptr, t x);
    int     _sizeMultiset(int8_t* ptr);
    int     _countMultiset(int8_t* ptr, t x);
    int8_t* _unionMultiset(int8_t* ptr1, int8_t* ptr2);
    int8_t* _intersectMultiset(int8_t* ptr1, int8_t* ptr2);
    int8_t* _differenceMultiset(int8_t* ptr1, int8_t* ptr2);
    int     _includesMultiset(int8_t* ptr1, int8_t* ptr2);
    void    _freeMultiset(int8_t* ptr);
    
    /* Function */
    int8_t* _newFunction();
    int     _equalFunction(int8_t* ptr1, int8_t* ptr2);
    void    _insertFunction(int8_t* ptr, t key, t value);
    int     _sizeFunction(int8_t *ptr);
    int     _isElemFunction(int8_t* ptr, t key, t value);
    int8_t* _domFunction(int8_t* ptr);
    t       _pairFunction(int8_t* ptr, t k);
    int8_t* _compositionFunction(int8_t *ptr1, int8_t *ptr2);
    void    _freeFunction(int8_t* ptr);
    
    /* Relation */
    int8_t* _newRelation();
    int     _equalRelation(int8_t* ptr1, int8_t* ptr2);
    void    _insertRelation(int8_t* ptr, t key, t value);
    int     _sizeRelation(int8_t *ptr);
    int     _isElemRelation(int8_t* ptr, t key, t value);
    int8_t* _domRelation(int8_t* ptr);
    int8_t* _pairRelation(int8_t* ptr, t key);
    int8_t* _compositionRelation(int8_t *ptr1, int8_t *ptr2);
    void    _freeRelation(int8_t* ptr);
    
    /* Sequence */
    int8_t* _newSequence();
    int     _equalSequence(int8_t* ptr1, int8_t* ptr2);
    int     _isElemSequence(int8_t* ptr, t x);
    void    _insertSequence(int8_t* ptr, t x);
    int8_t* _concatSequence(int8_t* ptr1, int8_t* ptr2);
    int     _sizeSequence(int8_t* ptr);
    void    _freeSequence(int8_t* ptr);
    
    /* TrashCollector 
     *  Every pointer to a type created (set, multiset, ...) is
     *  store inside a vector of pointer, to be freed when freeGarbage()
     *  is called.
     */
    void _initTrashCollector();
    void _openScope();
    void _closeScope();
    void _freeTrashCollector();
    
#ifdef __cplusplus
}
#endif
#endif /* Header_h */
