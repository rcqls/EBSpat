#ifndef _ebcache_h
#define _ebcache_h
#include "ebfunc.h"

#define DOUBLE long double

//CACHE for VECTOR
typedef struct st_cache_vector {
  DOUBLE* *cache;
  int nbCache,firstNeg;//firstNeg is the intermediate index for negative contribution!
} ST_CACHE_VECTOR, *CACHE_VECTOR;
extern CACHE_VECTOR ebcache_vector_new();
extern void ebcache_vector_free();

//CACHE for FUNC
typedef struct st_cache_func {
  FUNC func;
  //for creation
  CACHE_VECTOR* *cache;
  //int *weight; //for cumulative stuff!
  //short weighted; //is the method use weight? (1=yes,0=no)
  int nRow,cptRow;//number of x! (number of col is specified by func!)
  int nCol;
} ST_CACHE_FUNC,*CACHE_FUNC;
extern CACHE_FUNC ebcache_func_new();
extern void ebcache_func_alloc_cache();
extern void ebcache_func_init_systSampling();
extern void ebcache_func_init_dvsSum();
extern void ebcache_func_free_cache();
extern void ebcache_func_free();
extern void ebcache_func_print();
extern void ebcache_func_matrix();
extern void ebcache_func_compute();
extern SEXP ebcache_func_sum_code();
extern void ebcache_func_new_dv();
extern void ebcache_func_current_dv();

//CACHE for FUNC Global
typedef struct st_cache_funcGlob {
  FUNC func;
  //for creation 
  CACHE_VECTOR *cache;
} ST_CACHE_FUNC_GLOB,*CACHE_FUNC_GLOB;
extern CACHE_FUNC_GLOB ebcache_funcGlob_new();
extern void ebcache_funcGlob_free();
extern void ebcache_funcGlob_print();
extern void ebcache_funcGlob_init();



//CACHE of weighted vector!
typedef struct st_cache_weighted_vector {
  FUNC func;
  CQLS_VECTOR *vector;
  int nbVector; //cumulated vector[i]->nbVector
  //int *nbVector;
  DOUBLE* *cache;
  int nbCache;
  int *weight;
}ST_CACHE_WEIGHTED_VECTOR,*CACHE_WEIGHTED_VECTOR,*CACHE_W_VECTOR;
extern CACHE_W_VECTOR ebcache_wVector_new();
extern void ebcache_wVector_free();
extern void ebcache_wVector_realloc();

typedef struct st_cache_comp_func {
  FUNC func;
  CACHE_W_VECTOR cache; //cache size=func->nbTerms!
}ST_CACHE_COMP_FUNC,*CACHE_COMP_FUNC;
extern CACHE_COMP_FUNC ebcache_compFunc_new();
extern void ebcache_compFunc_free();
extern void ebcache_compFunc_print();
extern void ebcache_compFunc_init_systSampling();
extern void ebcache_compFunc_init_randSampling();
extern void ebcache_compFunc_init_dvsSum();
extern void ebcache_compFunc_print();
extern SEXP ebcache_compFunc_to_R_as_dataframe();
#endif
