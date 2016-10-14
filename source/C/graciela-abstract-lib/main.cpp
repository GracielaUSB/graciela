//
//  main.cpp
//  TestSuite
//
//  Created by Carlos Spaggiari Roa on 8/30/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#define CATCH_CONFIG_MAIN

#include "graciela-abstract-lib.h"
#include "catch.hpp"

typedef int8_t* Set;
typedef int8_t* Multiset;
typedef int8_t* Relation;
typedef int8_t* Function;
typedef int8_t* TrashCollector;



TEST_CASE("Graciela Relation"){
    _initTrashCollector();
    _openScope();
    Relation r = _newRelation();
    
    _insertRelation(r, 1, 2);
    _insertRelation(r, 1, 2); //duplicate
    _insertRelation(r, 2, 1);
    _insertRelation(r, 2, 1); //duplicate
    _insertRelation(r, 3, 3);
    _insertRelation(r, 3, 2);
    _insertRelation(r, 3, 1);
    
    REQUIRE(_sizeRelation(r) == 5);
    REQUIRE(_isElemRelation(r, 1, 2));
    REQUIRE(_isElemRelation(r, 2, 1));
    REQUIRE(_isElemRelation(r, 3, 3));
    REQUIRE(_isElemRelation(r, 3, 2));
    REQUIRE(_isElemRelation(r, 3, 1));
    
    Set dom = _domRelation(r);
    REQUIRE(_sizeSet(dom) == 3);
    REQUIRE(_isElemSet(dom, 1));
    REQUIRE(_isElemSet(dom, 2));
    REQUIRE(_isElemSet(dom, 3));
    Set rango = _pairRelation(r, 1);
    REQUIRE(_sizeSet(rango) == 1);
    REQUIRE(_isElemSet(rango,2));
    rango = _pairRelation(r, 2);
    REQUIRE(_sizeSet(rango) == 1);
    REQUIRE(_isElemSet(rango,1));
    rango = _pairRelation(r, 3);
    REQUIRE(_sizeSet(rango) == 3);
    REQUIRE(_isElemSet(rango,1));
    REQUIRE(_isElemSet(rango,2));
    REQUIRE(_isElemSet(rango,3));
    
    
    Relation r2 = _newRelation();
    _insertRelation(r2, 2, 10);
    _insertRelation(r2, 1, 20);
    _insertRelation(r2, 3, 30);
    
    
    /* Composition
     *  1,2           1,10
     *  2,1   1,20    2,20
     *  3,1 o 2,10 -> 3,10
     *  3,2   3,30    3,20
     *  3,3           3,30
     */
    Set s = _compositionRelation(r, r2);
    REQUIRE(_sizeRelation(s) == 5);
    REQUIRE(_isElemRelation(s, 1, 10));
    REQUIRE(_isElemRelation(s, 2, 20));
    REQUIRE(_isElemRelation(s, 3, 10));
    REQUIRE(_isElemRelation(s, 3, 20));
    REQUIRE(_isElemRelation(s, 3, 30));
    
    s = _compositionRelation(r2, r);
    REQUIRE(_sizeRelation(s) == 0);
    
    _freeTrashCollector();
}

TEST_CASE("Graciela Function"){
    _initTrashCollector();
    _openScope();
    Function f = _newFunction();
    _insertFunction(f, 1, 2);
    _insertFunction(f, 1, 2);
    _insertFunction(f, 2, 1);
    _insertFunction(f, 2, 1);
    _insertFunction(f, 3, 3);
    _insertFunction(f, 3, 2);
    _insertFunction(f, 3, 1);
  
    REQUIRE(_sizeFunction(f) == 3);
    REQUIRE(_isElemFunction(f, 1, 2));
    REQUIRE(_isElemFunction(f, 2, 1));
    REQUIRE(_isElemFunction(f, 3, 3));
    REQUIRE_FALSE(_isElemFunction(f, 3, 2));
    REQUIRE_FALSE(_isElemFunction(f, 3, 1));

    Set s = _domFunction(f);
    REQUIRE(_isElemSet(s, 1));
    REQUIRE(_isElemSet(s, 2));
    REQUIRE(_isElemSet(s, 3));
    
    REQUIRE(_pairFunction(f, 1, 1, 1) == 2);
    REQUIRE(_pairFunction(f, 2, 1, 1) == 1);
    REQUIRE(_pairFunction(f, 3, 1, 1) == 3);
    
    Function f2 = _newFunction();
    _insertFunction(f2, 2, 10);
    _insertFunction(f2, 1, 20);
    _insertFunction(f2, 3, 30);
    
    
    /* Composition
     *  1,2   1,20    1,10
     *  2,1 o 2,10 -> 2,20
     *  3,1   3,30    3,10
     */
    s = _compositionFunction(f, f2);
    REQUIRE(_sizeFunction(s) == 3);
    REQUIRE(_isElemFunction(s, 1, 10));
    REQUIRE(_isElemFunction(s, 2, 20));
    REQUIRE(_isElemFunction(s, 3, 30));
    
    s = _compositionRelation(f2, f);
    REQUIRE(_sizeFunction(s) == 0);
  Set set = _newSetPair();
  gtuple t1 = {1,2};
  gtuple t2 = {2,4};
  gtuple t3 = {3,8};
  gtuple t4 = {4,16};
  _insertSetPair(set, &t1);
  _insertSetPair(set, &t2);
  _insertSetPair(set, &t3);
  _insertSetPair(set, &t4);
  
  Function fun = _funcFromSet(set, 1, 1);
  
    _freeTrashCollector();
}

TEST_CASE("Graciela Multiset"){
    
    _initTrashCollector();
    _openScope();
    Multiset empty = _newMultiset();
    Multiset s = _newMultiset();
    REQUIRE(_sizeMultiset(s) == 0);
    _insertMultiset(s, 1);
    _insertMultiset(s, 1);
    _insertMultiset(s, 2);
    _insertMultiset(s, 2);
    _insertMultiset(s, 2);
    REQUIRE(_sizeMultiset(s) == 5);
    REQUIRE(_isElemMultiset(s, 1));
    REQUIRE(_isElemMultiset(s, 2));
    REQUIRE(_countMultiset(1,s) == 2);
    REQUIRE(_countMultiset(2,s) == 3);
    
    Multiset s2 = _newMultiset();
    _insertMultiset(s2, 2);
    _insertMultiset(s2, 2);
    _insertMultiset(s2, 2);
    REQUIRE(_includesMultiset(s, s2));
    _insertMultiset(s2, 3);
    _insertMultiset(s2, 3);
    REQUIRE_FALSE(_includesMultiset(s, s2));
    REQUIRE(_sizeMultiset(s2) == 5);
    
    /* Union                                       */
    /* {1,1,2,2,2} u {2,2,2,3,3} = {1,1,2,2,2,3,3} */
    Multiset unionS = _unionMultiset(s, s2);
    REQUIRE(_sizeMultiset(unionS) == 7);
    REQUIRE(_isElemMultiset(unionS, 1));
    REQUIRE(_isElemMultiset(unionS, 2));
    REQUIRE(_isElemMultiset(unionS, 3));
    REQUIRE(_countMultiset(1,unionS) == 2);
    REQUIRE(_countMultiset(2,unionS) == 3);
    REQUIRE(_countMultiset(3,unionS) == 2);

    unionS = _unionMultiset(s2, s);
    REQUIRE(_sizeMultiset(unionS) == 7);
    REQUIRE(_isElemMultiset(unionS, 1));
    REQUIRE(_isElemMultiset(unionS, 2));
    REQUIRE(_isElemMultiset(unionS, 3));
    REQUIRE(_countMultiset(1,unionS) == 2);
    REQUIRE(_countMultiset(2,unionS) == 3);
    REQUIRE(_countMultiset(3,unionS) == 2);

    
    
    /* Intersection                            */
    /* {1,1,2,2,2} ∩ {2,2,2,2,2,3,3} = {2,2,2} */
    _insertMultiset(s2, 2);
    _insertMultiset(s2, 2);
    Multiset intersectS = _intersectMultiset(s, s2);
    REQUIRE(_sizeMultiset(intersectS) == 3);
    REQUIRE(_isElemMultiset(intersectS, 2));
    REQUIRE(_countMultiset(2,intersectS) == 3);

    intersectS = _intersectMultiset(s2, s);
    REQUIRE(_sizeMultiset(intersectS) == 3);
    REQUIRE(_isElemMultiset(intersectS, 2));
    REQUIRE(_countMultiset(2,intersectS) == 3);
    
    /* {1,1,2,2,2} ∩ {} */
    intersectS = _intersectMultiset(s, empty);
    REQUIRE(_sizeMultiset(intersectS) == 0);
    intersectS = _intersectMultiset(empty, s);
    REQUIRE(_sizeMultiset(intersectS) == 0);
    
    /* Difference */
    /* {1,1,2,2,2} \ {}  */
    Multiset differenceS = _differenceMultiset(s, empty);
    REQUIRE(_sizeMultiset(differenceS) == 5);
    REQUIRE(_countMultiset(1,differenceS) == 2);
    REQUIRE(_countMultiset(2,differenceS) == 3);
    differenceS = _differenceMultiset(empty, s);
    REQUIRE(_sizeMultiset(differenceS) == 0);
    
    /* {1,1,2,2,2} \ {2,2,2,2,2,3,3} = {1,1} */
    differenceS = _differenceMultiset(s, s2);
    REQUIRE(_sizeMultiset(differenceS) == 2);
    REQUIRE(_countMultiset(1,differenceS) == 2);
    
    /* {2,2,2,2,2,3,3} \ {1,1,2,2,2} = {2,2,3,3} */
    differenceS = _differenceMultiset(s2, s);
    REQUIRE(_sizeMultiset(differenceS) == 4);
    REQUIRE(_countMultiset(2,differenceS) == 2);
    REQUIRE(_countMultiset(3,differenceS) == 2);
    
    _freeTrashCollector();
}

TEST_CASE("Graciela Set"){
    /* Create set, insert element and is element */
    _initTrashCollector();
  
    _openScope();
    Set s = _newSet();
    REQUIRE(_equalSet(_unionSet(s,s), s));

    REQUIRE(_sizeSet(s) == 0);
    _insertSet(s, 1);
    _insertSet(s, 1);
    _insertSet(s, 1);
    _insertSet(s, 2);
    _insertSet(s, 2);
    _insertSet(s, 2);
    _insertSet(s, 3);
    _insertSet(s, 4);
    REQUIRE(_sizeSet(s) == 4);
    REQUIRE(_isElemSet(s, 1));
    REQUIRE(_isElemSet(s, 2));
    REQUIRE(_isElemSet(s, 3));
    REQUIRE(_isElemSet(s, 4));
    _insertSet(s, 1);
    _insertSet(s, 2);
    _insertSet(s, 3);
    _insertSet(s, 5);
    REQUIRE(_sizeSet(s) == 5);
    REQUIRE(_isElemSet(s, 1));
    REQUIRE(_isElemSet(s, 2));
    REQUIRE(_isElemSet(s, 3));
    REQUIRE(_isElemSet(s, 4));
    REQUIRE(_isElemSet(s, 5));
    
    /* Union */
    Set s2 = _newSet();
    _insertSet(s2, 1);
    REQUIRE(_includesSet(s, s2));
    _insertSet(s2, 2);
    REQUIRE(_includesSet(s, s2));
    _insertSet(s2, 3);
    REQUIRE(_includesSet(s, s2));
    _insertSet(s2, 4);
    REQUIRE(_includesSet(s, s2));
    _insertSet(s2, 5);
    REQUIRE(_includesSet(s, s2));
    Set unionS = _unionSet(s, s2);
    REQUIRE(_sizeSet(unionS) == 5);
    _insertSet(s2, 6);
    
    unionS = _unionSet(s, s2);
    REQUIRE(_sizeSet(unionS) == 6);

    
    /* Intersect */
    Set intersectS = _intersectSet(s, s2);
    REQUIRE(_sizeSet(intersectS) == 5);
    REQUIRE_FALSE(_isElemSet(intersectS, 6));

    intersectS = _intersectSet(s2, s);
    REQUIRE(_sizeSet(intersectS) == 5);
    REQUIRE_FALSE(_isElemSet(intersectS, 6));

    
    /* Difference */
    Set differenceS = _differenceSet(s, s2);
    REQUIRE(_sizeSet(differenceS) == 0);

    differenceS = _differenceSet(s2, s);
    REQUIRE(_sizeSet(differenceS) == 1);
    REQUIRE(_isElemSet(differenceS, 6));
    _insertSet(s2, 6);
    REQUIRE_FALSE(_includesSet(s, s2));
    REQUIRE(_includesSet(s2, s));
  
  Set a = _newSet(), b = _newSet();
  _insertSet(a, 4);
  _insertSet(b, 4);
  REQUIRE(_equalSet(_differenceSet(a, b), _newSet()));
  
    _freeTrashCollector();
}
