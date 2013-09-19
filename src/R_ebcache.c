#include "ebvor.h"
#include "ebfunc.h"
#include "ebcache.h"
#include <stdlib.h>
#include <math.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/PrtUtil.h>

#include "R_ebspat.h"

//Cache
void* C_ebcache_reactivate(SEXP self) {
  FUNC func;
  SEXP funcR;
  CACHE_FUNC cache; 

#ifdef debugMode
  Rprintf("Reactivating ebcache ...\n");
#endif
  //reactivation of func
  PROTECT(funcR=C_getExternalPtrForExternalPtr(self,"func"));
  func=C_ebfunc_reactivate(funcR);
  R_SetExternalPtrAddr(funcR, func);
#ifdef debugMode
  Rprintf("ebcache reactivated!\n");
#endif
  cache=ebcache_func_new(func);
  //DEBUG:
#ifdef debugMode
  Rprintf("ebcache pt=%p\n",cache);
#endif
  UNPROTECT(1);
  return cache;
}

SEXP R_ebcache_reactivate(SEXP args) {
  CACHE_FUNC cache;

  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  //DEBUG:
#ifdef debugMode
  Rprintf("R reactivate: cache pt=%p\n",cache);
#endif
  return R_NilValue;
}

void C_ebcache_free(SEXP self) {
  CACHE_FUNC cache;
  //Rprintf("GC Cache is in use!\n");
  cache=(CACHE_FUNC)R_ExternalPtrAddr(self);
  if(cache==NULL) return;
  ebcache_func_free(cache);
  R_ClearExternalPtr(self);
}


SEXP R_ebcache_new(SEXP args) {
  FUNC func;
  CACHE_FUNC cache;
  SEXP self;
  
  func=(FUNC) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CADR(args)),C_ebfunc_reactivate);
  //printf("newCache!:%d\n",func);
  cache=ebcache_func_new(func); //printf("done!\n");
  //printf("cache pt=%d\n",cache);
  self=R_MakeExternalPtr(cache, R_NilValue, R_NilValue);
  //R_RegisterCFinalizerEx(self,(R_CFinalizer_t)C_ebcache_free, TRUE);  
  return self;
}

SEXP R_ebcacheSystSampl_init(SEXP args) {
  PT_POLY poly;
  CACHE_FUNC cache;
  int nb;
  DOUBLE domainSize[2];
  
  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  nb=INTEGER(CADDR(args))[0];
  domainSize[0]=(DOUBLE)(REAL(CADDDR(args))[0]);
  domainSize[1]=(DOUBLE)(REAL(CADDDR(args))[1]);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CAD4R(args)),C_ebpoly_reactivate);
//printf("poly=%p,\n",poly);
//printf("newCache!:%d\n",cache);
  ebcache_func_init_systSampling(cache,nb,domainSize,poly); //Rprintf("done!\n");
  return R_NilValue;
}

SEXP R_ebcacheRandSampl_init(SEXP args) {
  PT_POLY poly;
  CACHE_FUNC cache;
  int nb;
  DOUBLE domainSize[2];
  
  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  nb=INTEGER(CADDR(args))[0];
  domainSize[0]=(DOUBLE)(REAL(CADDDR(args))[0]);
  domainSize[1]=(DOUBLE)(REAL(CADDDR(args))[1]);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CAD4R(args)),C_ebpoly_reactivate);
//printf("poly=%p,\n",poly);
  //printf("newCache!:%d\n",cache);
  ebcache_func_init_randSampling(cache,nb,domainSize,poly); //Rprintf("done!\n");
  return R_NilValue;
}


SEXP R_ebcacheSum_init(SEXP args) {
  PT_POLY poly;
  CACHE_FUNC cache;
  DOUBLE domainSize[2];
  
  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  domainSize[0]=(DOUBLE)(REAL(CADDR(args))[0]);
  domainSize[1]=(DOUBLE)(REAL(CADDR(args))[1]);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CADDDR(args)),C_ebpoly_reactivate);
  //printf("newCache!:%d\n",func);
  ebcache_func_init_dvsSum(cache,domainSize,poly); //printf("done!\n");
  return R_NilValue;
}


SEXP R_ebcache_print(SEXP args) {
  CACHE_FUNC cache;

  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  //DEBUG:
#ifdef debugMode
  Rprintf("print:cache pt=%p\n",cache);
#endif
  ebcache_func_print(cache);//printf("printed!\n");
  return R_NilValue;
}

SEXP R_ebcache_compute(SEXP args) {
  CACHE_FUNC cache;
  SEXP init,code;

  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  PROTECT(init=CADDR(args));
  PROTECT(code=CADDDR(args));
  ebcache_func_compute(cache,init,code);//printf("computed!\n");
  UNPROTECT(2);
  return R_NilValue;
}

SEXP R_ebcache_sum_code(SEXP args) {
  CACHE_FUNC cache;
  SEXP resR,code;

  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  PROTECT(code=CADDR(args));
  resR=ebcache_func_sum_code(cache,code);//Rprintf("computed=%LF!\n",res);
  UNPROTECT(1);
  return resR;
}

SEXP R_ebcache_matrix(SEXP args) {
  CACHE_FUNC cache;
  DOUBLE* mat;
  SEXP matR;
  int i,n;
 
  cache=(CACHE_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcache_reactivate);
  n=cache->nRow * cache->nCol;
  mat=(DOUBLE*)Calloc(n,DOUBLE);
  ebcache_func_matrix(cache,mat);//printf("computed!\n");
  PROTECT(matR=allocMatrix(REALSXP,cache->nRow,cache->nCol));
  for(i=0;i<n;i++) REAL(matR)[i]=mat[i];
  Free(mat);
  UNPROTECT(1);
  return matR;
}


SEXP R_ebcache_free(SEXP args) {
  CACHE_FUNC cache;
  cache=(CACHE_FUNC) R_ExternalPtrAddr(CADR(args));
  if(cache==(CACHE_FUNC)NULL) {
#ifdef debugMode
    Rprintf("EBCache object already freed!\n");
#endif
  } else { 
    ebcache_func_free(cache);//printf("freed!\n");
    R_SetExternalPtrAddr(CADR(args), (CACHE_FUNC)NULL);
#ifdef debugMode
    Rprintf("EBCache object freed!\n");
#endif
  }
  return R_NilValue;
}

//Just a test function!
SEXP R_ebcache_test(SEXP args) {
  FUNC func;
  PT_POLY poly;
  CACHE_FUNC cache;
  int nb;
  DOUBLE domainSize;
  
  func=(FUNC) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CADR(args)),C_ebfunc_reactivate);
  nb=INTEGER(CADDR(args))[0];
  domainSize=(DOUBLE)(REAL(CADDDR(args))[0]);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CAD4R(args)),C_ebpoly_reactivate);
  //printf("newCache!:%d\n",func);
  cache=ebcache_func_new(func);
  ebcache_func_init_systSampling(cache,nb,domainSize,poly); //printf("done!\n");
  ebcache_func_print(cache);
  ebcache_func_compute(cache);
  ebcache_func_free(cache);
  return R_NilValue;
}

//CACHE_COMP_FUNC
void C_ebcacheCompFunc_free(SEXP self) {
  CACHE_COMP_FUNC cache;
#ifdef debugMode
  Rprintf("GC CacheCompFunc  is in use!\n");
#endif
  cache=(CACHE_COMP_FUNC)R_ExternalPtrAddr(self);
  if(cache==NULL) return;
  ebcache_compFunc_free(cache);
  R_ClearExternalPtr(self);
}

SEXP R_ebcacheCompFunc_new(SEXP args) {
  FUNC func;
  CACHE_COMP_FUNC cache;
  SEXP self;
  
  func=(FUNC) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CADR(args)),C_ebfunc_reactivate);
  //printf("newCache!:%p\n",func);
  cache=ebcache_compFunc_new(func); //printf("done!\n");
  //printf("cache pt=%p\n",cache);
  self=R_MakeExternalPtr(cache, R_NilValue, R_NilValue);
  //TODO: to debug! R_RegisterCFinalizerEx(self,(R_CFinalizer_t)C_ebcacheCompFunc_free, TRUE);  
  return self;
}

SEXP R_ebcacheCompFunc_free(SEXP args) {
  CACHE_COMP_FUNC cache;
  cache=(CACHE_COMP_FUNC) R_ExternalPtrAddr(CADR(args));
  if(cache==(CACHE_COMP_FUNC)NULL) {
#ifdef debugMode
    Rprintf("EBCacheCompFunc object already freed!\n");
#endif
  } else {
    ebcache_compFunc_free(cache);//printf("freed!\n");
    R_SetExternalPtrAddr(CADR(args), (CACHE_COMP_FUNC)NULL);
#ifdef debugMode
    Rprintf("EBCacheCompFunc object freed!\n");
#endif
  }
  return R_NilValue;
}

SEXP R_ebcacheCompFunc_initSystSampl(SEXP args) {
  PT_POLY poly;
  CACHE_COMP_FUNC cache;
  int nb;
  DOUBLE domainSize[2];
  
  cache=(CACHE_COMP_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcacheCompFunc_reactivate);
  nb=INTEGER(CADDR(args))[0];
  domainSize[0]=(DOUBLE)(REAL(CADDDR(args))[0]);
  domainSize[1]=(DOUBLE)(REAL(CADDDR(args))[1]);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CAD4R(args)),C_ebpoly_reactivate);
//printf("poly=%p,\n",poly);
//printf("newCache!:%p\n",cache);
  ebcache_compFunc_init_systSampling(cache,nb,domainSize,poly); //Rprintf("done!\n");
  return R_NilValue;
}

SEXP R_ebcacheCompFunc_initRandSampl(SEXP args) {
  PT_POLY poly;
  CACHE_COMP_FUNC cache;
  int nb;
  DOUBLE domainSize[2];
  
  cache=(CACHE_COMP_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcacheCompFunc_reactivate);
  nb=INTEGER(CADDR(args))[0];
  domainSize[0]=(DOUBLE)(REAL(CADDDR(args))[0]);
  domainSize[1]=(DOUBLE)(REAL(CADDDR(args))[1]);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CAD4R(args)),C_ebpoly_reactivate);
//printf("poly=%p,\n",poly);
  //printf("newCache!:%d\n",cache);
  ebcache_compFunc_init_randSampling(cache,nb,domainSize,poly); //Rprintf("done!\n");
  return R_NilValue;
}


SEXP R_ebcacheCompFunc_initSum(SEXP args) {
  PT_POLY poly;
  CACHE_COMP_FUNC cache;
  DOUBLE domainSize[2];
  
  cache=(CACHE_COMP_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcacheCompFunc_reactivate);
  domainSize[0]=(DOUBLE)(REAL(CADDR(args))[0]);
  domainSize[1]=(DOUBLE)(REAL(CADDR(args))[1]);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CADDDR(args)),C_ebpoly_reactivate);
  //printf("newCache!:%d\n",func);
  ebcache_compFunc_init_dvsSum(cache,domainSize,poly); //printf("done!\n");
  return R_NilValue;
}

void* C_ebcacheCompFunc_reactivate(SEXP self) {
  FUNC func;
  SEXP funcR;
  CACHE_COMP_FUNC cache; 

#ifdef debugMode
  Rprintf("Reactivating ebcacheCompFunc ...\n");
#endif
  //reactivation of func
  PROTECT(funcR=C_getVarForExternalPtr(self,"func"));
  func=(FUNC) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(funcR),C_ebfunc_reactivate);
  cache=ebcache_compFunc_new(func);
#ifdef debugMode
  Rprintf("ebcacheCompFunc reactivated!\n");
#endif
  //DEBUG:
#ifdef debugMode
  Rprintf("ebcacheCompFunc pt=%p\n",cache);
#endif
  UNPROTECT(1);
  return cache;
}

SEXP R_ebcacheCompFunc_reactivate(SEXP args) {
  CACHE_COMP_FUNC cache;

  cache=(CACHE_COMP_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcacheCompFunc_reactivate);
  //DEBUG:
#ifdef debugMode
  Rprintf("R reactivate: cacheCompFunc pt=%p\n",cache);
#endif
  return R_NilValue;
}

SEXP R_ebcacheCompFunc_print(SEXP args) {
  CACHE_COMP_FUNC cache;

  cache=(CACHE_COMP_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcacheCompFunc_reactivate);
  //DEBUG: Rprintf("print:cacheCompFunc pt=%p\n",cache);
  ebcache_compFunc_print(cache);//printf("printed!\n");
  return R_NilValue;
}

SEXP R_ebcacheCompFunc_as_dataframe(SEXP args) {
  CACHE_COMP_FUNC cache;

  cache=(CACHE_COMP_FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebcacheCompFunc_reactivate);
//DEBUG: Rprintf("list of dataframe:cacheCompFunc pt=%p\n",cache);
  return ebcache_compFunc_to_R_as_dataframe(cache); 
}



/*
void* C_ebcache_reactivate(SEXP self) {
  FUNC func;
  PT_POLY poly;
  SEXP tmp;
  int cacheType,nbGridPts;
  DOUBLE domainSize;
  CACHE_FUNC cache; 

  Rprintf("Reactivating ebcache ...\n");
  //reactivation of func
  tmp=getAttrib(self,install("func"));
  func=C_ebfunc_reactivate(tmp);
  R_SetExternalPtrAddr(tmp, func);
  //reactivation of poly
  tmp=getAttrib(self,install("poly"));
  poly=C_ebpoly_reactivate(tmp);
  R_SetExternalPtrAddr(tmp, poly);
  PROTECT(tmp=coerceVector(getAttrib(self,install("domainSize")),REALSXP));
  domainSize=REAL(tmp)[0];
  UNPROTECT(1);
  PROTECT(tmp=coerceVector(getAttrib(self,install("cacheType")),INTSXP));
  cacheType=INTEGER(tmp)[0];
  if(cacheType==1) {
    PROTECT(tmp=coerceVector(getAttrib(self,install("nbGridPts")),INTSXP));
    //printf("nbmax=%d\n",INTEGER(tmp)[0]);
    nbGridPts=INTEGER(tmp)[0];
    UNPROTECT(1);
    cache=ebcache_func_new_systSampling(func,nbGridPts,domainSize,poly); 
  } else if(cacheType==2) {
    cache=ebcache_func_new_dvsSum(func,domainSize,poly); 
  }
  UNPROTECT(1);
  Rprintf("ebcache reactivated!\n");
  Rprintf("ebcache pt=%d\n",cache);
  return cache;
}*/
