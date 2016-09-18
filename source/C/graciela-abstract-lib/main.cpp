//
//  main.
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
    initTC();
    openScope();
    Relation r = newRelation();
    
    insertRelation(r, 1, 2);
    insertRelation(r, 1, 2); //duplicate
    insertRelation(r, 2, 1);
    insertRelation(r, 2, 1); //duplicate
    insertRelation(r, 3, 3);
    insertRelation(r, 3, 2);
    insertRelation(r, 3, 1);
    
    REQUIRE(sizeRelation(r) == 5);
    REQUIRE(isElemRelation(r, 1, 2));
    REQUIRE(isElemRelation(r, 2, 1));
    REQUIRE(isElemRelation(r, 3, 3));
    REQUIRE(isElemRelation(r, 3, 2));
    REQUIRE(isElemRelation(r, 3, 1));
    
    Set dom = domRelation(r);
    REQUIRE(sizeSet(dom) == 3);
    REQUIRE(isElemSet(dom, 1));
    REQUIRE(isElemSet(dom, 2));
    REQUIRE(isElemSet(dom, 3));
    Set rango = pairRelation(r, 1);
    REQUIRE(sizeSet(rango) == 1);
    REQUIRE(isElemSet(rango,2));
    rango = pairRelation(r, 2);
    REQUIRE(sizeSet(rango) == 1);
    REQUIRE(isElemSet(rango,1));
    rango = pairRelation(r, 3);
    REQUIRE(sizeSet(rango) == 3);
    REQUIRE(isElemSet(rango,1));
    REQUIRE(isElemSet(rango,2));
    REQUIRE(isElemSet(rango,3));
    
    
    Relation r2 = newRelation();
    insertRelation(r2, 2, 10);
    insertRelation(r2, 1, 20);
    insertRelation(r2, 3, 30);
    
    
    /* Composition
     *  1,2           1,10
     *  2,1   1,20    2,20
     *  3,1 o 2,10 -> 3,10
     *  3,2   3,30    3,20
     *  3,3           3,30
     */
    Set s = compositionRelation(r, r2);
    REQUIRE(sizeRelation(s) == 5);
    REQUIRE(isElemRelation(s, 1, 10));
    REQUIRE(isElemRelation(s, 2, 20));
    REQUIRE(isElemRelation(s, 3, 10));
    REQUIRE(isElemRelation(s, 3, 20));
    REQUIRE(isElemRelation(s, 3, 30));
    
    s = compositionRelation(r2, r);
    REQUIRE(sizeRelation(s) == 0);
    
    freeTrashCollector();
}

TEST_CASE("Graciela Function"){
    initTC();
    openScope();
    Function f = newFunction();
    insertFunction(f, 1, 2);
    insertFunction(f, 1, 2);
    insertFunction(f, 2, 1);
    insertFunction(f, 2, 1);
    insertFunction(f, 3, 3);
    insertFunction(f, 3, 2);
    insertFunction(f, 3, 1);
    
    REQUIRE(sizeFunction(f) == 3);
    REQUIRE(isElemFunction(f, 1, 2));
    REQUIRE(isElemFunction(f, 2, 1));
    REQUIRE(isElemFunction(f, 3, 3));
    REQUIRE_FALSE(isElemFunction(f, 3, 2));
    REQUIRE_FALSE(isElemFunction(f, 3, 1));

    Set s = domFunction(f);
    REQUIRE(isElemSet(s, 1));
    REQUIRE(isElemSet(s, 2));
    REQUIRE(isElemSet(s, 3));
    
    REQUIRE(pairFunction(f, 1) == 2);
    REQUIRE(pairFunction(f, 2) == 1);
    REQUIRE(pairFunction(f, 3) == 3);
    
    Function f2 = newFunction();
    insertFunction(f2, 2, 10);
    insertFunction(f2, 1, 20);
    insertFunction(f2, 3, 30);
    
    
    /* Composition
     *  1,2   1,20    1,10
     *  2,1 o 2,10 -> 2,20
     *  3,1   3,30    3,10
     */
    s = compositionFunction(f, f2);
    REQUIRE(sizeFunction(s) == 3);
    REQUIRE(isElemFunction(s, 1, 10));
    REQUIRE(isElemFunction(s, 2, 20));
    REQUIRE(isElemFunction(s, 3, 30));
    
    s = compositionRelation(f2, f);
    REQUIRE(sizeFunction(s) == 0);
    freeTrashCollector();
}

TEST_CASE("Graciela Multiset"){
    
    initTC();
    openScope();
    Multiset empty = newMultiset();
    Multiset s = newMultiset();
    REQUIRE(sizeMultiset(s) == 0);
    insertMultiset(s, 1);
    insertMultiset(s, 1);
    insertMultiset(s, 2);
    insertMultiset(s, 2);
    insertMultiset(s, 2);
    REQUIRE(sizeMultiset(s) == 5);
    REQUIRE(isElemMultiset(s, 1));
    REQUIRE(isElemMultiset(s, 2));
    REQUIRE(countMultiset(s, 1) == 2);
    REQUIRE(countMultiset(s, 2) == 3);
    
    Multiset s2 = newMultiset();
    insertMultiset(s2, 2);
    insertMultiset(s2, 2);
    insertMultiset(s2, 2);
    insertMultiset(s2, 3);
    insertMultiset(s2, 3);
    REQUIRE(sizeMultiset(s2) == 5);
    
    /* Union                                       */
    /* {1,1,2,2,2} u {2,2,2,3,3} = {1,1,2,2,2,3,3} */
    Multiset unionS = unionMultiset(s, s2);
    REQUIRE(sizeMultiset(unionS) == 7);
    REQUIRE(isElemMultiset(unionS, 1));
    REQUIRE(isElemMultiset(unionS, 2));
    REQUIRE(isElemMultiset(unionS, 3));
    REQUIRE(countMultiset(unionS, 1) == 2);
    REQUIRE(countMultiset(unionS, 2) == 3);
    REQUIRE(countMultiset(unionS, 3) == 2);

    unionS = unionMultiset(s2, s);
    REQUIRE(sizeMultiset(unionS) == 7);
    REQUIRE(isElemMultiset(unionS, 1));
    REQUIRE(isElemMultiset(unionS, 2));
    REQUIRE(isElemMultiset(unionS, 3));
    REQUIRE(countMultiset(unionS, 1) == 2);
    REQUIRE(countMultiset(unionS, 2) == 3);
    REQUIRE(countMultiset(unionS, 3) == 2);

    
    
    /* Intersection                            */
    /* {1,1,2,2,2} ∩ {2,2,2,2,2,3,3} = {2,2,2} */
    insertMultiset(s2, 2);
    insertMultiset(s2, 2);
    Multiset intersectS = intersectMultiset(s, s2);
    REQUIRE(sizeMultiset(intersectS) == 3);
    REQUIRE(isElemMultiset(intersectS, 2));
    REQUIRE(countMultiset(intersectS, 2) == 3);

    intersectS = intersectMultiset(s2, s);
    REQUIRE(sizeMultiset(intersectS) == 3);
    REQUIRE(isElemMultiset(intersectS, 2));
    REQUIRE(countMultiset(intersectS, 2) == 3);
    
    /* {1,1,2,2,2} ∩ {} */
    intersectS = intersectMultiset(s, empty);
    REQUIRE(sizeMultiset(intersectS) == 0);
    intersectS = intersectMultiset(empty, s);
    REQUIRE(sizeMultiset(intersectS) == 0);
    
    /* Difference */
    /* {1,1,2,2,2} \ {}  */
    Multiset differenceS = differenceMultiset(s, empty);
    REQUIRE(sizeMultiset(differenceS) == 5);
    REQUIRE(countMultiset(differenceS, 1) == 2);
    REQUIRE(countMultiset(differenceS, 2) == 3);
    differenceS = differenceMultiset(empty, s);
    REQUIRE(sizeMultiset(differenceS) == 0);
    
    /* {1,1,2,2,2} \ {2,2,2,2,2,3,3} = {1,1} */
    differenceS = differenceMultiset(s, s2);
    REQUIRE(sizeMultiset(differenceS) == 2);
    REQUIRE(countMultiset(differenceS, 1) == 2);
    
    /* {2,2,2,2,2,3,3} \ {1,1,2,2,2} = {2,2,3,3} */
    differenceS = differenceMultiset(s2, s);
    REQUIRE(sizeMultiset(differenceS) == 4);
    REQUIRE(countMultiset(differenceS, 2) == 2);
    REQUIRE(countMultiset(differenceS, 3) == 2);
    
    freeTrashCollector();
}

TEST_CASE("Graciela Set"){
    /* Create set, insert element and is element */
    initTC();
    openScope();
    Set s = newSet();
    REQUIRE(sizeSet(s) == 0);
    insertSet(s, 1);
    insertSet(s, 1);
    insertSet(s, 1);
    insertSet(s, 2);
    insertSet(s, 2);
    insertSet(s, 2);
    insertSet(s, 3);
    insertSet(s, 4);
    REQUIRE(sizeSet(s) == 4);
    REQUIRE(isElemSet(s, 1));
    REQUIRE(isElemSet(s, 2));
    REQUIRE(isElemSet(s, 3));
    REQUIRE(isElemSet(s, 4));
    insertSet(s, 1);
    insertSet(s, 2);
    insertSet(s, 3);
    insertSet(s, 5);
    REQUIRE(sizeSet(s) == 5);
    REQUIRE(isElemSet(s, 1));
    REQUIRE(isElemSet(s, 2));
    REQUIRE(isElemSet(s, 3));
    REQUIRE(isElemSet(s, 4));
    REQUIRE(isElemSet(s, 5));
    
    /* Union */
    Set s2 = newSet();
    insertSet(s2, 1);
    insertSet(s2, 2);
    insertSet(s2, 3);
    insertSet(s2, 4);
    insertSet(s2, 5);
    Set unionS = unionSet(s, s2);
    REQUIRE(sizeSet(unionS) == 5);
    insertSet(s2, 6);
    
    unionS = unionSet(s, s2);
    REQUIRE(sizeSet(unionS) == 6);

    
    /* Intersect */
    Set intersectS = intersectSet(s, s2);
    REQUIRE(sizeSet(intersectS) == 5);
    REQUIRE_FALSE(isElemSet(intersectS, 6));

    intersectS = intersectSet(s2, s);
    REQUIRE(sizeSet(intersectS) == 5);
    REQUIRE_FALSE(isElemSet(intersectS, 6));

    
    /* Difference */
    Set differenceS = differenceSet(s, s2);
    REQUIRE(sizeSet(differenceS) == 0);

    differenceS = differenceSet(s2, s);
    REQUIRE(sizeSet(differenceS) == 1);
    REQUIRE(isElemSet(differenceS, 6));
    freeTrashCollector();
}
