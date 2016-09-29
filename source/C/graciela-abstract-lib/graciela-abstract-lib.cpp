//
//  graciela-abstract-lib.
//  graciela-abstract-lib
//
//  Created by Carlos Spaggiari Roa on 8/24/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#include "graciela-abstract-lib.h"

#ifdef __cplusplus


extern "C" {

    using namespace glib;

    int8_t* _stack;

    void mark(TCTuple t){
        ((stack<TrashCollector*>*)_stack)->top()->push_back(t);
    }

    /* Set */
    int8_t *_newSet(){
        Set *set = new Set;
        mark(TCTuple((int8_t*)set,SET));
        return (int8_t*)set;
    }

    Iterator * _first(int8_t *ptr){
        Set* set = (Set*)ptr;

        Set::iterator *it =(Set::iterator*)malloc (sizeof(Set::iterator));
        *it = set->begin();
        t data = **it;
        if (set->begin() == set->end()){
            free(it);
            return NULL;
        }
        else {
            Iterator *i = (Iterator*)malloc(sizeof(Iterator));
            i->data = data;
            i->it = (int8_t*) it;
            i->type = ptr;
            return i;
        }
    }
    Iterator *_next(Iterator* i){
        Set::iterator *next = (Set::iterator*)i->it;
        Set* type = (Set*)i->type;

        *next = ++(*next);
        if (*next != type->end()) {
            Iterator *_i = (Iterator*)malloc(sizeof(Iterator));
            _i->data = **next;
            _i->it = (int8_t*) next;
            _i->type = i->type;
            free(i);
            return _i;
        } else {
            free(i->it);
            free(i);
            return NULL;
        }
    }

    int _equalSet(int8_t *ptr1, int8_t* ptr2){
        return *(Set*)ptr1 == *(Set*)ptr2;
    }

    void _insertSet(int8_t *ptr, t x){
        Set *s = (Set*) ptr;
        s->insert(x);
    }

    int _sizeSet(int8_t *ptr) {
        return (int) ((Set*) ptr)->size();
    }

    int _isElemSet(int8_t *ptr, t x){
        Set *s = (Set*) ptr;
        return s->find(x) != s->end();
    }

    int8_t* _unionSet(int8_t *ptr1, int8_t * ptr2) {
        Set *set1   = (Set*)ptr1,
            *set2   = (Set*)ptr2,
            *newset = (Set*)_newSet();

        set_union(set1->begin(), set1->end(),
                  set2->begin(), set2->end(),
                  inserter(*newset, newset->begin()));

        return (int8_t*)newset;
    }

    int8_t* _intersectSet(int8_t *ptr1, int8_t * ptr2) {
        Set *set1   = (Set*)ptr1,
            *set2   = (Set*)ptr2,
            *newset = (Set*)_newSet();

        set_intersection(set1->begin(), set1->end(),
                         set2->begin(), set2->end(),
                         inserter(*newset, newset->begin()));

        return (int8_t*)newset;
    }

    int8_t* _differenceSet(int8_t *ptr1, int8_t * ptr2) {
        Set *set1   = (Set*)ptr1,
            *set2   = (Set*)ptr2,
            *newset = (Set*)_newSet();

        set_difference(set1->begin(), set1->end(),
                       set2->begin(), set2->end(),
                       inserter(*newset, newset->begin()));

        return (int8_t*)newset;
    }

    int _includesSet(int8_t* ptr1, int8_t* ptr2){
        Set *set1 = (Set*)ptr1,
            *set2 = (Set*)ptr2;

        return includes(set1->begin(), set1->end(),
                        set2->begin(), set2->end());
    }

    void _freeSet(int8_t* ptr) {
        delete (Set*)ptr;
    }

    /* SetPair */
    int8_t *_newSetPair(){
        SetPair *set = new SetPair;
        mark(TCTuple((int8_t*)set,SETPAIR));
        return (int8_t*)set;
    }

    int _equalSetPair(int8_t *ptr1, int8_t* ptr2){
        return *(SetPair*)ptr1 == *(SetPair*)ptr2;
    }

    void _insertSetPair(int8_t *ptr, gtuple x){
        SetPair *s = (SetPair*) ptr;

        s->insert(Tuple(x.a,x.b));
    }

    int _sizeSetPair(int8_t *ptr) {
        return (int) ((Set*) ptr)->size();
    }

    int _isElemSetPair(int8_t *ptr, gtuple x){
        SetPair *s = (SetPair*) ptr;
        return s->find(Tuple(x.a,x.b)) != s->end();
    }

    int8_t* _unionSetPair(int8_t *ptr1, int8_t * ptr2) {
        SetPair *set1   = (SetPair*)ptr1,
        *set2   = (SetPair*)ptr2,
        *newset = (SetPair*)_newSet();

        set_union(set1->begin(), set1->end(),
                  set2->begin(), set2->end(),
                  inserter(*newset, newset->begin()));

        return (int8_t*)newset;
    }

    int8_t* _intersectSetPair(int8_t *ptr1, int8_t * ptr2) {
        SetPair *set1   = (SetPair*)ptr1,
        *set2   = (SetPair*)ptr2,
        *newset = (SetPair*)_newSet();

        set_intersection(set1->begin(), set1->end(),
                         set2->begin(), set2->end(),
                         inserter(*newset, newset->begin()));

        return (int8_t*)newset;
    }

    int8_t* _differenceSetPair(int8_t *ptr1, int8_t * ptr2) {
        SetPair *set1   = (SetPair*)ptr1,
        *set2   = (SetPair*)ptr2,
        *newset = (SetPair*)_newSet();

        set_difference(set1->begin(), set1->end(),
                       set2->begin(), set2->end(),
                       inserter(*newset, newset->begin()));

        return (int8_t*)newset;
    }

    int _includesSetPair(int8_t* ptr1, int8_t* ptr2){
        Set *set1 = (Set*)ptr1,
            *set2 = (Set*)ptr2;

        return includes(set1->begin(), set1->end(),
                        set2->begin(), set2->end());
    }

    void _freeSetPair(int8_t* ptr) {
        delete (SetPair*)ptr;
    }

    /* MultiSet */

    int8_t* _newMultiset(){
        Multiset *mul = new Multiset;
        mark(TCTuple((int8_t*)mul,MULTISET));
        return (int8_t*)mul;
    }

    int _equalMultiset(int8_t *ptr1, int8_t* ptr2){
        return *(Multiset*)ptr1 == *(Multiset*)ptr2;

    }

    void _insertMultiset(int8_t *ptr, t x){
        Multiset *s = (Multiset*) ptr;
        s->insert(x);
    }

    int _isElemMultiset(int8_t *ptr, t x){
        Multiset *s = (Multiset*) ptr;
        return s->find(x) != s->end();
    }

    int _sizeMultiset(int8_t *ptr){
        return (int) ((Multiset*) ptr)->size();
    }

    int _countMultiset(int8_t *ptr, t x){
        Multiset *s = (Multiset*) ptr;
        return (int) s->count(x);
    }

    int8_t* _unionMultiset(int8_t *ptr1, int8_t * ptr2) {
        Multiset *set1   = (Multiset*)ptr1,
                 *set2   = (Multiset*)ptr2,
                 *newSet = (Multiset*)_newMultiset();

        set_union(set1->begin(), set1->end(),
                  set2->begin(), set2->end(),
                  inserter(*newSet, newSet->begin()));

        return (int8_t*)newSet;
    }

    int8_t* _intersectMultiset(int8_t *ptr1, int8_t * ptr2) {
        Multiset *set1   = (Multiset*)ptr1,
                 *set2   = (Multiset*)ptr2,
                 *newSet = (Multiset*)_newMultiset();

        set_intersection(set1->begin(), set1->end(),
                         set2->begin(), set2->end(),
                         inserter(*newSet, newSet->begin()));

        return (int8_t*)newSet;
    }

    int8_t* _differenceMultiset(int8_t *ptr1, int8_t * ptr2) {
        Multiset *set1   = (Multiset*)ptr1,
                 *set2   = (Multiset*)ptr2,
                 *newSet = (Multiset*)_newMultiset();

        set_difference(set1->begin(), set1->end(),
                       set2->begin(), set2->end(),
                       inserter(*newSet, newSet->begin()));

        return (int8_t*)newSet;
    }

    int _includesMultiset(int8_t* ptr1, int8_t* ptr2){
        Multiset *set1 = (Multiset*)ptr1,
                 *set2 = (Multiset*)ptr2;

        return includes(set1->begin(), set1->end(),
                        set2->begin(), set2->end());
    }

    void _freeMultiset(int8_t* ptr) {
        delete (Multiset*)ptr;
    }


    /* Function */

    int8_t *_newFunction(){
        Function *function = new Function();
        mark(TCTuple((int8_t*)function,FUNCTION));
        return (int8_t*) function;
    }

    int _equalFunction(int8_t* ptr1, int8_t* ptr2){
        return *(Function*)ptr1 == *(Function*)ptr2;
    }

    void _insertFunction(int8_t *ptr, t key, t value) {
        ((Function*)ptr)->insert(Tuple(key,value));
    }

    int8_t *_funcFromSet(int8_t* setPtr){
        SetPair* set = (SetPair*)setPtr;
        int8_t *func = _newFunction();
        for(SetPair::iterator it = set->begin(); it != set->end(); ++it){
            ((Function*)func)->insert(*it);
        }
        return func;
    }

    int _sizeFunction(int8_t *ptr){
        return (int) ((Function*)ptr)->size();
    }

    int _isElemFunction(int8_t *ptr, t key, t value) {
        Function *function = (Function*)ptr;
        Function::iterator it = function->find(key);

        if (function->find(key) != function->end())
        {
            return (it->second == value);
        }

        return false;
    }

    int8_t *_domFunction(int8_t *ptr){
        Function *function = (Function*)ptr;
        Set      *set = (Set*)_newSet();
        for(Function::iterator it = function->begin(); it != function->end(); ++it){
            set->insert(it->first);
        }
        return (int8_t*) set;
    }

    t _pairFunction(int8_t *ptr, t k){
        Function *function = (Function*)ptr;
        return (t)function->find(k)->second;
    }

    int8_t *_compositionFunction(int8_t *ptr1, int8_t *ptr2){
        Function *f1      = (Function*)ptr1,
                 *f2      = (Function*)ptr2,
                 *newFunc = (Function*)_newFunction();

        for(Function::iterator it = f1->begin(); it != f1->end(); ++it){

            Function::iterator it2 = f2->find(it->second);
            if ( it2 != f2->end())
                newFunc->insert(Tuple(it->first,it2->second));
        }

        return (int8_t*)newFunc;
    }

    void _freeFunction(int8_t* ptr) {
        delete (Function*)ptr;
    }

    /* Relation */


    int8_t *_newRelation(){
        Relation *rel = new Relation();
        mark(TCTuple((int8_t*)rel,RELATION));
        return (int8_t*) rel;
    }

    int _equalRelation(int8_t *ptr1, int8_t *ptr2){
        return *(Relation*)ptr1 == *(Relation*)ptr2;
    }

    void _insertRelation(int8_t *ptr, t key, t value) {
        ((Relation*)ptr)->insert(Tuple(key,value));
    }

    int8_t *_relationFromSet(int8_t* setPtr){
        SetPair* set = (SetPair*)setPtr;
        int8_t *rel = _newRelation();
        for(SetPair::iterator it = set->begin(); it != set->end(); ++it){
            ((Function*)rel)->insert(*it);
        }
        return rel;
    }

    int _sizeRelation(int8_t *ptr){
        return (int) ((Relation*)ptr)->size();
    }

    int _isElemRelation(int8_t *ptr, t key, t value) {
        Relation *rel = (Relation*)ptr;
        return rel->find(Tuple(key,value)) != rel->end();
    }

    int8_t* _domRelation(int8_t *ptr){
        Relation *rel = (Relation*)ptr;
        Set      *set = (Set*)_newSet();
        for(Relation::iterator it = rel->begin(); it != rel->end(); ++it){
            set->insert(it->first);
        }
        return (int8_t*) set;
    }

    int8_t* _pairRelation(int8_t *ptr, t key){
        Relation *rel = (Relation*)ptr;
        Set      *set = (Set*)_newSet();

        for(Relation::iterator it = rel->begin(); it != rel->end(); ++it){
            if (it->first == key)
                set->insert(it->second);
        }
        return (int8_t*) set;
    }

    int8_t *_compositionRelation(int8_t *ptr1, int8_t *ptr2){
        Relation *r1     = (Relation*)ptr1,
                 *newRel = (Relation*)_newRelation();


        for(Relation::iterator it = r1->begin(); it != r1->end(); ++it){

            Set *rge2 = (Set*)_pairRelation(ptr2, it->second);
            for(Set::iterator it2 = rge2->begin(); it2 != rge2->end(); ++it2){
                newRel->insert(Tuple(it->first,*it2));
            }
        }

        return (int8_t*)newRel;
    }

    void _freeRelation(int8_t* ptr) {
        delete (Relation*)ptr;
    }

    /* Sequence */

    int8_t *_newSequence(){
        Sequence *s = new Sequence;
        mark(TCTuple((int8_t*)s,SEQUENCE));
        return (int8_t*) s;
    }

    int _equalSequence(int8_t* ptr1, int8_t* ptr2){
        return *(Sequence*)ptr1 == *(Sequence*)ptr2;
    }

    int _isElemSequence(int8_t *ptr, t x){
        Sequence *seq = (Sequence*)ptr;
        for (Sequence::iterator it = seq->begin() ; it != seq->end() ; ++it){
            if (*it == x) return true;
        }
        return false;
    }

    void _insertSequence(int8_t* ptr, t x){
        ((Sequence*)ptr)->push_back(x);
    }

    int8_t* _concatSequence(int8_t* ptr1, int8_t* ptr2){
      Sequence *seq1   = (Sequence*)ptr1,
               *seq2   = (Sequence*)ptr2,
               *newSeq = (Sequence*)_newSequence();
      newSeq->insert( newSeq->end(), seq1->begin(), seq1->end());
      newSeq->insert( newSeq->end(), seq2->begin(), seq2->end());
      return (int8_t*)newSeq;
    }

    int _sizeSequence(int8_t* ptr){
        return (int) ((Sequence*)ptr)->size();
    }

    void _freeSequence(int8_t* ptr) {
        delete (Sequence*)ptr;
    }

    /* Trash Collector */

    void _initTrashCollector(){
        _stack = (int8_t*)(new stack<TrashCollector*>);
    }

    void _openScope(){
        TrashCollector *tc = new TrashCollector;
        tc->reserve(64);
        ((stack<TrashCollector*>*)_stack)->push(tc);
    }

    void _closeScope(){
        TrashCollector* tc = ((stack<TrashCollector*>*)_stack)->top();
        ((stack<TrashCollector*>*)_stack)->pop();
        for (TrashCollector::iterator it = tc[0].begin(); it != tc[0].end(); ++it){
            switch (it->second) {
                case SET: {
                    _freeSet(it->first);
                    break;
                }
                case MULTISET: {
                    _freeMultiset(it->first);
                    break;
                }
                case FUNCTION: {
                    _freeFunction(it->first);
                    break;
                }
                case RELATION: {
                    _freeRelation(it->first);
                    break;
                }
                case SEQUENCE: {
                    _freeSequence(it->first);
                    break;
                }
                case SETPAIR: {
                    _freeSetPair(it->first);
                    break;
                }
            }
        }
        tc->clear();
        delete tc;
    }

    void _freeTrashCollector(){
        for (int i = 0 ; i < ((stack<TrashCollector*>*)_stack)->size(); i++)
        {
            _closeScope();
        }

        delete ((stack<TrashCollector*>*)_stack);
    }
}



#endif
