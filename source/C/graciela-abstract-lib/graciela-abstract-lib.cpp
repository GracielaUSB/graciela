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
    int8_t *newSet(){
        Set *set = new Set;
        mark(TCTuple((int8_t*)set,SET));
        return (int8_t*)set;
    }
    
    Iterator * first(int8_t *ptr){
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
    Iterator *next(Iterator* i){
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
    
    int equalSet(int8_t *ptr1, int8_t* ptr2){
        return (Set*)ptr1 == (Set*)ptr2;
    }
    
    void insertSet(int8_t *ptr, t x){
        Set *s = (Set*) ptr;
        s->insert(x);
    }
    
    int sizeSet(int8_t *ptr) {
        return (int) ((Set*) ptr)->size();
    }
    
    int isElemSet(int8_t *ptr, t x){
        Set *s = (Set*) ptr;
        return s->find(x) != s->end();
    }
    
    int8_t* unionSet(int8_t *ptr1, int8_t * ptr2) {
        Set *set1   = (Set*)ptr1,
            *set2   = (Set*)ptr2,
            *newset = (Set*)newSet();
        
        set_union(set1->begin(), set1->end(),
                  set2->begin(), set2->end(),
                  inserter(*newset, newset->begin()));
        
        return (int8_t*)newset;
    }
    
    int8_t* intersectSet(int8_t *ptr1, int8_t * ptr2) {
        Set *set1   = (Set*)ptr1,
            *set2   = (Set*)ptr2,
            *newset = (Set*)newSet();
        
        set_intersection(set1->begin(), set1->end(),
                         set2->begin(), set2->end(),
                         inserter(*newset, newset->begin()));
        
        return (int8_t*)newset;
    }
    
    int8_t* differenceSet(int8_t *ptr1, int8_t * ptr2) {
        Set *set1   = (Set*)ptr1,
            *set2   = (Set*)ptr2,
            *newset = (Set*)newSet();
        
        set_difference(set1->begin(), set1->end(),
                       set2->begin(), set2->end(),
                       inserter(*newset, newset->begin()));
        
        return (int8_t*)newset;
    }
    
    void freeSet(int8_t* ptr) {
        delete (Set*)ptr;
    }
    
    /* MultiSet */
    
    int8_t *newMultiset(){
        Multiset *mul = new Multiset;
        mark(TCTuple((int8_t*)mul,MULTISET));
        return (int8_t*)mul;
    }
    
    int equalMultiset(int8_t *ptr1, int8_t* ptr2){
        return (Multiset*)ptr1 == (Multiset*)ptr2;
        
    }
    
    void insertMultiset(int8_t *ptr, t x){
        Multiset *s = (Multiset*) ptr;
        s->insert(x);
    }
    
    int isElemMultiset(int8_t *ptr, t x){
        Multiset *s = (Multiset*) ptr;
        return s->find(x) != s->end();
    }
    
    int sizeMultiset(int8_t *ptr){
        return (int) ((Multiset*) ptr)->size();
    }
    
    int countMultiset(int8_t *ptr, t x){
        Multiset *s = (Multiset*) ptr;
        return (int) s->count(x);
    }
    
    int8_t* unionMultiset(int8_t *ptr1, int8_t * ptr2) {
        Multiset *set1   = (Multiset*)ptr1,
                 *set2   = (Multiset*)ptr2,
                 *newSet = (Multiset*)newMultiset();
        
        set_union(set1->begin(), set1->end(),
                  set2->begin(), set2->end(),
                  inserter(*newSet, newSet->begin()));
        
        return (int8_t*)newSet;
    }
    
    int8_t* intersectMultiset(int8_t *ptr1, int8_t * ptr2) {
        Multiset *set1   = (Multiset*)ptr1,
                 *set2   = (Multiset*)ptr2,
                 *newSet = (Multiset*)newMultiset();

        set_intersection(set1->begin(), set1->end(),
                         set2->begin(), set2->end(),
                         inserter(*newSet, newSet->begin()));
        
        return (int8_t*)newSet;
    }
    
    int8_t* differenceMultiset(int8_t *ptr1, int8_t * ptr2) {
        Multiset *set1   = (Multiset*)ptr1,
                 *set2   = (Multiset*)ptr2,
                 *newSet = (Multiset*)newMultiset();
        
        set_difference(set1->begin(), set1->end(),
                       set2->begin(), set2->end(),
                       inserter(*newSet, newSet->begin()));

        return (int8_t*)newSet;
    }
    
    void freeMultiset(int8_t* ptr) {
        delete (Multiset*)ptr;
    }
    
    
    /* Function */
    
    int8_t *newFunction(){
        Function *function = new Function();
        mark(TCTuple((int8_t*)function,FUNCTION));
        return (int8_t*) function;
    }
    
    int equalFunction(int8_t* ptr1, int8_t* ptr2){
        return (Function*)ptr1 == (Function*)ptr2;
    }
    
    void insertFunction(int8_t *ptr, t key, t value) {
        ((Function*)ptr)->insert(Tuple(key,value));
    }
    
    int sizeFunction(int8_t *ptr){
        return (int) ((Function*)ptr)->size();
    }
    
    int isElemFunction(int8_t *ptr, t key, t value) {
        Function *function = (Function*)ptr;
        Function::iterator it = function->find(key);
        
        if (function->find(key) != function->end())
        {
            return (it->second == value);
        }
        
        return false;
    }
    
    int8_t *domFunction(int8_t *ptr){
        Function *function = (Function*)ptr;
        Set      *set = (Set*)newSet();
        for(Function::iterator it = function->begin(); it != function->end(); ++it){
            set->insert(it->first);
        }
        return (int8_t*) set;
    }
    
    int pairFunction(int8_t *ptr, t k){
        Function *function = (Function*)ptr;
        return (int)function->find(k)->second;
    }
    
    int8_t *compositionFunction(int8_t *ptr1, int8_t *ptr2){
        Function *f1      = (Function*)ptr1,
                 *f2      = (Function*)ptr2,
                 *newFunc = (Function*)newFunction();
        
        for(Function::iterator it = f1->begin(); it != f1->end(); ++it){
            
            Function::iterator it2 = f2->find(it->second);
            if ( it2 != f2->end())
                newFunc->insert(Tuple(it->first,it2->second));
        }
        
        return (int8_t*)newFunc;
    }
    
    void freeFunction(int8_t* ptr) {
        delete (Function*)ptr;
    }
    
    /* Relation */
    
    
    int8_t *newRelation(){
        Relation *rel = new Relation();
        mark(TCTuple((int8_t*)rel,RELATION));
        return (int8_t*) rel;
    }
    
    int equalRelation(int8_t *ptr1, int8_t *ptr2){
        return (Relation*)ptr1 == (Relation*)ptr2;
    }
    
    void insertRelation(int8_t *ptr, t key, t value) {
        ((Relation*)ptr)->insert(Tuple(key,value));
    }
    
    int sizeRelation(int8_t *ptr){
        return (int) ((Relation*)ptr)->size();
    }
    
    int isElemRelation(int8_t *ptr, t key, t value) {
        Relation *rel = (Relation*)ptr;
        return rel->find(Tuple(key,value)) != rel->end();
    }
    
    int8_t* domRelation(int8_t *ptr){
        Relation *rel = (Relation*)ptr;
        Set      *set = (Set*)newSet();
        for(Relation::iterator it = rel->begin(); it != rel->end(); ++it){
            set->insert(it->first);
        }
        return (int8_t*) set;
    }
    
    int8_t* pairRelation(int8_t *ptr, t key){
        Relation *rel = (Relation*)ptr;
        Set      *set = (Set*)newSet();
        
        for(Relation::iterator it = rel->begin(); it != rel->end(); ++it){
            if (it->first == key)
                set->insert(it->second);
        }
        return (int8_t*) set;
    }
    
    int8_t *compositionRelation(int8_t *ptr1, int8_t *ptr2){
        Relation *r1     = (Relation*)ptr1,
                 *newRel = (Relation*)newRelation();
        

        for(Relation::iterator it = r1->begin(); it != r1->end(); ++it){
            
            Set *rge2 = (Set*)pairRelation(ptr2, it->second);
            for(Set::iterator it2 = rge2->begin(); it2 != rge2->end(); ++it2){
                newRel->insert(Tuple(it->first,*it2));
            }
        }
        
        return (int8_t*)newRel;
    }
    
    void freeRelation(int8_t* ptr) {
        delete (Relation*)ptr;
    }
    
    /* Sequence */
    
    int8_t *newSequence(){
        Sequence *s = new Sequence;
        mark(TCTuple((int8_t*)s,SEQUENCE));
        return (int8_t*) s;
    }
    
    int equalSequence(int8_t* ptr1, int8_t* ptr2){
        return (Sequence*)ptr1 == (Sequence*)ptr2;
    }
    
    int isElemSequence(int8_t *ptr, t x){
        Sequence *seq = (Sequence*)ptr;
        for (Sequence::iterator it = seq->begin() ; it != seq->end() ; ++it){
            if (*it == x) return true;
        }
        return false;
    }
    
    void insertSequence(int8_t* ptr, t x){
        ((Sequence*)ptr)->push_back(x);
    }
    
    int sizeSequence(int8_t* ptr){
        return (int) ((Sequence*)ptr)->size();
    }
    
    void freeSequence(int8_t* ptr) {
        delete (Sequence*)ptr;
    }
    
    /* Trash Collector */
    
    void initTC(){
        _stack = (int8_t*)(new stack<TrashCollector>);
    }
    
    void openScope(){
        TrashCollector *tc = new TrashCollector;
        tc->reserve(64);
        ((stack<TrashCollector*>*)_stack)->push(tc);
    }
    
    void closeScope(){
        TrashCollector* tc = ((stack<TrashCollector*>*)_stack)->top();
        ((stack<TrashCollector*>*)_stack)->pop();
        for (TrashCollector::iterator it = tc[0].begin(); it != tc[0].end(); ++it){
            switch (it->second) {
                case SET: {
                    freeSet(it->first);
                    break;
                }
                case MULTISET: {
                    freeMultiset(it->first);
                    break;
                }
                case FUNCTION: {
                    freeFunction(it->first);
                    break;
                }
                case RELATION: {
                    freeRelation(it->first);
                    break;
                }
                case SEQUENCE: {
                    freeSequence(it->first);
                    break;
                }
            }
        }
        tc->clear();
        delete tc;
    }
    
    void freeTrashCollector(){
        for (int i = 0 ; i < ((stack<TrashCollector*>*)_stack)->size(); i++)
        {
            closeScope();
        }

        delete ((stack<TrashCollector*>*)_stack);
    }
}



#endif