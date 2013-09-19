#include "ebcache.h"
#include "ebfunc.h"

//allocation and initialization in one step! local and global freed at the end!
CACHE_VECTOR ebcache_vector_new(FUNC_VECTOR_CACHE cache) {
  CACHE_VECTOR self;
  PT_LIST *pt;
  int i,j;

  self=(CACHE_VECTOR)Calloc(1,ST_CACHE_VECTOR);
  self->firstNeg=eblist_length(cache->list->list);
  self->nbCache=self->firstNeg+eblist_length(cache->oldList->list);
  if(self->nbCache > 0) {
    self->cache = (DOUBLE**)Calloc(self->nbCache,DOUBLE*);
    i=0;pt=&(cache->list->list);
    while(*pt!=NULL) self->cache[i++]=(DOUBLE*)eblist_recup_tete(pt);//permet de vider la list sans vider les éléments!
    pt=&(cache->oldList->list);
    while(*pt!=NULL) self->cache[i++]=(DOUBLE*)eblist_recup_tete(pt);
  } else self->cache=(DOUBLE**)NULL;
/*  //Glob
  if(self->nbCacheGlob > 0) {
    self->nbCacheGlob=eblist_length(cacheGlob->list->list);
    self->cacheGlob = (DOUBLE**)Calloc(self->nbCacheGlob,DOUBLE*);
    i=0;pt=&(cacheGlob->list->list);
    while(*pt!=NULL) self->cacheGlob[i++]=(DOUBLE*)eblist_recup_tete(pt);
  } else self->cacheGlob=(DOUBLE**)NULL;*/
  return self;
}

void ebcache_vector_free(CACHE_VECTOR self) {
int i;

  if(self!=(CACHE_VECTOR)NULL) {
    //empty all the stuff!
    for(i=0;i<self->nbCache;i++) Free(self->cache[i]);Free(self->cache);
    //for(i=0;i<self->nbCacheGlob;i++) Free(self->cacheGlob[i]);Free(self->cacheGlob);
    Free(self);
  }
} 

void  ebcache_vector_print(CACHE_VECTOR self,CQLS_VECTOR vector) {
  int i;

  for(i=0;i<self->nbCache;i++) {  
    //printf("pt:%d\n",pt);  
    Rprintf("%d",i+1);
    if(i<self->firstNeg) Rprintf("+: "); else Rprintf("-: ");
//printf("self->cache[%d]=%LF\n",i,self->cache[i][0]);
    cqlsVector_print(vector,self->cache[i]);
  }
}

void  ebcache_vector_updateInc(CACHE_VECTOR self,CQLS_VECTOR vector,DOUBLE* res,CQLS_R envR) {
  int i;
  DOUBLE sgn=1.0;
  
  for(i=0;i<self->nbCache;i++) {  
    if(i==self->firstNeg) sgn=-1.0; //from self->firstNeg substract instead of add!
    cqlsVector_vector_increased_by_weighted_vector(vector, res, self->cache[i],sgn);
  }
  //for debug: cqlsVector_print(vector,res);
  
}

void  ebcache_vector_update(CACHE_VECTOR self,CQLS_VECTOR vector,CQLS_VECTOR sum,CQLS_R envR) {
  int i;
  DOUBLE sgn;

  sgn=1.0;
  
  cqlsVector_initCache(sum);//all values to 0!
  for(i=0;i<self->nbCache;i++) {  
    if(i==self->firstNeg) sgn=-1.0; //from self->firstNeg substract instead of add!
    //printf("self->cache[%d]=%d,envR=%d\n",i,self->cache[i],envR);
    //printf("update i=%d\n",i);
    //put the value in self->cache[i] in the R system!
    cqlsVector_from_vector_to_envR(vector, self->cache[i],envR);//printf("titi2");//
    cqlsVector_cache_increased_by_weighted_updated_vector_from_envR(sum,sgn,envR);//printf("titi3");
  }

/*  cqlsVector_initCache(sumGlob);//all values to 0!
  for(i=0;i<self->nbCacheGlob;i++) {
    //printf("self->cacheGlob[%d]=%d,envR=%d\n",i,self->cacheGlob[i],envR);
    //printf("update i=%d\n",i);
    cqlsVector_from_vector_to_envR(vectorGlob, self->cacheGlob[i],envR);//printf("titi2");//
    cqlsVector_cache_increased_by_weighted_updated_vector_from_envR(sumGlob,sgn,envR);//printf("titi3");
  }*/
}
 
//CACHE for FUNC
CACHE_FUNC ebcache_func_new(FUNC func) {
  CACHE_FUNC self;

  self=(CACHE_FUNC)Calloc(1,ST_CACHE_FUNC);
  self->func=func;
  //for creation 
  self->nRow=0; //number of points 0 first
  self->nCol=ebfunc_ncol(self->func);
  Rprintf("ebcache ncol=%d\n",self->nCol);
  self->cache=(CACHE_VECTOR**)NULL;
  //self->weighted=0;
  //self->weight=(int*)NULL;
  return self;
}

void ebcache_func_alloc_cache(CACHE_FUNC self,int nb) {
  if(self->cache != (CACHE_VECTOR**)NULL) ebcache_func_free_cache(self); 
  self->cache=(CACHE_VECTOR**)Calloc(nb,CACHE_VECTOR*);
  //self->weight=(int*)NULL;
  self->nRow=nb;
  Rprintf("cache row number=%d\n",self->nRow);
}

//use to incrementally realloc
void ebcache_func_realloc_cache(CACHE_FUNC self,int nb) {
  self->cache=(CACHE_VECTOR**)Realloc(self->cache,nb,CACHE_VECTOR*);
  //self->weight=(int*)Realloc(self->weight,nb,int);
  (self->nRow)++;
  //Rprintf("cache row number=%d\n",self->nRow);
}

void ebcache_func_free_cache(CACHE_FUNC self) {
  int i,j;

  for(i=0;i<self->nRow;i++) {
    for(j=0;j<self->func->nbTermList;j++) ebcache_vector_free(self->cache[i][j]);
    if(self->cache[i] != (CACHE_VECTOR*)NULL) Free(self->cache[i]);
  }
  //if(self->weight != (int*)NULL) Free(self->weight);
  if(self->cache != (CACHE_VECTOR**)NULL) Free(self->cache);        
}

void ebcache_func_free(CACHE_FUNC self) {
  int i,j;

  if(self!=(CACHE_FUNC)NULL) {
    ebcache_func_free_cache(self);
    Free(self);          
  }
  self=(CACHE_FUNC)NULL;

}

/*void ebcache_func_set_weighted(CACHE_FUNC self,short weighted) {

}*/

void ebcache_func_init_systSampling(CACHE_FUNC self,int nbGridPts,DOUBLE *domainSize,PT_POLY poly) {
  int i,nb;
  DOUBLE x,y,pas[2];
  PT_DV pt_dv;

  //if(!self->weighted) {
  nb=nbGridPts*nbGridPts;
  ebcache_func_alloc_cache(self,nb);
  //}
  self->cptRow=0;
  pas[0]=domainSize[0]/(DOUBLE)(nbGridPts);pas[1]=domainSize[1]/(DOUBLE)(nbGridPts);
  Rprintf("nb=%d,domainSize=(%LF,%LF)\n",nb,domainSize[0],domainSize[1]);
  for(x=poly->vg->center[0]-domainSize[0]/2.0+pas[0]/2.0;x<=poly->vg->center[0]+domainSize[0]/2.0;x+=pas[0]) 
  for(y=poly->vg->center[1]-domainSize[1]/2.0+pas[1]/2.0;y<=poly->vg->center[1]+domainSize[1]/2.0;y+=pas[1]) {
    //printf("x=%LF,y=%LF\n",x,y);
    pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(poly->vg);
    (*pt_dv)->point[0]=x;
    (*pt_dv)->point[1]=y;
    //marks generation
    if(poly->vg->marks->nbData) {//there is marks!
      DC_curObj(poly->vg->marks->dc,*pt_dv);
      //TODO: TO FIX! 
      DU_cqlsSEXP_getAllVars_List(cqlsRExpr_eval(poly->vg->marksGen),poly->vg->marks);
    }
    //printf("make ins-> ?\n");
    ebpoly_make_ins(poly,pt_dv); //printf("done! new cache point ?\n");
    ebcache_func_new_dv(self,poly);//printf("new cache point done\n");
    ebpoly_final_ins(poly);//IMPORTANT: the point is not inserted. Cancel is done in the calculation of interactions in EBFunc!
  }
}

void ebcache_func_init_randSampling(CACHE_FUNC self,int nb,DOUBLE *domainSize,PT_POLY poly) {
  int i;
  PT_DV pt_dv;

  ebcache_func_alloc_cache(self,nb);
  self->cptRow=0;
  Rprintf("nb=%d,domainSize=(%LF,%LF)\n",nb,domainSize[0],domainSize[1]);
  GetRNGstate();
  for(i=0;i<nb;i++) {
    pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(poly->vg);
    (*pt_dv)->point[0]=(DOUBLE)(unif_rand()*domainSize[0] - domainSize[0]/2.0 + poly->vg->center[0]);
    (*pt_dv)->point[1]=(DOUBLE)(unif_rand()*domainSize[1] - domainSize[1]/2.0 + poly->vg->center[1]);
    //Rprintf("pt_dv[%d]=(%LF,%LF)\n",i,(*pt_dv)->point[0],(*pt_dv)->point[1]);
    //marks generation
    if(poly->vg->marks->nbData) {//there is marks!
      DC_curObj(poly->vg->marks->dc,*pt_dv);
      //TODO: TO FIX! 
      DU_cqlsSEXP_getAllVars_List(cqlsRExpr_eval(poly->vg->marksGen),poly->vg->marks);
    }
    //printf("make ins-> ?\n");
    ebpoly_make_ins(poly,pt_dv); //printf("done! new cache point ?\n");
    ebcache_func_new_dv(self,poly);//printf("new cache point done\n");
    ebpoly_final_ins(poly);//IMPORTANT: the point is not inserted. Cancel is done in the calculation of interactions in EBFunc!  
  }
  PutRNGstate();
}



void ebcache_func_init_dvsSum(CACHE_FUNC self,DOUBLE *domainSize,PT_POLY poly) {
  int i,nb;
  PT_DV pt_dv;

  /*size of the cache is the number of point inside the domain!
  nb=0;
  for(i=0;i<poly->vg->nb_dv;i++) {
    pt_dv=poly->vg->tab_dv+i;
    ///printf("point=%d\n",*pt_dv);
    if(*pt_dv==(DV)NULL) continue;
    if(ebdv_dans_domaine(pt_dv,poly->vg->center,domainSize)) nb++;
  }*/

  nb=ebpoly_nbPoints_inside(poly,domainSize);
  Rprintf("nb=%d, domainSize(%LF,%LF)=%d\n",nb,domainSize[0],domainSize[1]);
  ebcache_func_alloc_cache(self,nb);

  self->cptRow=0;
  for(i=0;i<poly->vg->nb_dv;i++) {
    pt_dv=poly->vg->tab_dv+i;
    //Rprintf("point=%p\n",*pt_dv);
    if(*pt_dv==(DV)NULL) continue;
    //if((*pt_dv)->num_s==-3) continue;
    if(!ebdv_dans_domaine(pt_dv,poly->vg->center,domainSize)) continue;
    //Rprintf("point=%p in domain\n",*pt_dv);
    ebpoly_make_sup(poly,pt_dv);
    ebcache_func_new_dv(self,poly);
    ebpoly_final_sup(poly);//IMPORTANT: the point is not deleted. Cancel is done in the calculation of interactions in EBFunc!
  }

}

int ebcache_func_index_for_vector_in_cache(CACHE_FUNC self,DOUBLE *vector) {
  int i,j,idTerm,cpt;
  FUNC_TERM_WRAP termWrap;
  DOUBLE* tmp;  

  for(i=0;i<self->nRow;i++) {
    tmp=vector;
    cpt=0;
    for(j=0;j<self->func->nbTermList;j++) {
      idTerm=self->func->termList[j];
      termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
      //compare the element of vector with cache element!
      if(cqlsVector_same_vectors(termWrap->term->function->cacheLoc,self->cache[i][j],tmp)) cpt++;
      else break;
    }
    tmp += termWrap->term->function->vectorLoc->nbVector;
    if(cpt==self->func->nbTermList) return i; //found in the cache!
  }
  return -1; //not found in the cache!
}


void ebcache_func_new_dv(CACHE_FUNC self,PT_POLY poly) {//poly: no need as a field!
  int j,idTerm;
  FUNC_TERM_WRAP termWrap;

  //printf("np deb:%d\n",self->func);
  //by assuming the point is tried outside
  //Rmk: below 1 stands for a local cache
  ebfunc_componentCacheNew(self->func,1); // Rprintf("np new\n");//all the ebfunc_cache_vector are created!
  ebfunc_componentCacheUpdate(self->func,poly); // Rprintf("np updated\n");//now updated!
  //
  self->cache[self->cptRow]=(CACHE_VECTOR*)Calloc(self->func->nbTermList,CACHE_VECTOR); //printf("np allocated\n");
  for(j=0;j<self->func->nbTermList;j++) {
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    self->cache[self->cptRow][j]=ebcache_vector_new(termWrap->term->function->cacheLoc);
    //printf("self->cache[%d][%d]:\n",self->cptRow,j);
    //ebcache_vector_print(self->cache[self->cptRow][j],termWrap->term->function->vectorLoc);
  }
  //printf("np caches vector new\n");
  ebfunc_componentCacheFree(self->func);//printf("np freed\n"); //free all the ebfunc_cache_vector
  (self->cptRow)++;
}

/*void ebcache_func_current_dv(CACHE_FUNC self,PT_POLY poly,int row) {//poly: no need as a field!
  int j,idTerm;
  FUNC_TERM_WRAP termWrap;

  //printf("np deb:%d\n",self->func);
  //by assuming the point is tried outside
  //Rmk: below 1 stands for a local cache
  self->cptRow=row;
  ebfunc_componentCacheNew(self->func,1); // Rprintf("np new\n");//all the ebfunc_cache_vector are created!
  ebfunc_componentCacheUpdate(self->func,poly); // Rprintf("np updated\n");//now updated!
  //
  self->cache[self->cptRow]=(CACHE_VECTOR*)Calloc(self->func->nbTermList,CACHE_VECTOR); //printf("np allocated\n");
  for(j=0;j<self->func->nbTermList;j++) {
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    self->cache[self->cptRow][j]=ebcache_vector_new(termWrap->term->function->cacheLoc);
    //printf("self->cache[%d][%d]:\n",self->cptRow,j);
    //ebcache_vector_print(self->cache[self->cptRow][j],termWrap->term->function->vectorLoc);
  }
  //printf("np caches vector new\n");
  ebfunc_componentCacheFree(self->func);//printf("np freed\n"); //free all the ebfunc_cache_vector
  
}*/



void ebcache_func_print(CACHE_FUNC self) {
  int i,j,idTerm;
  FUNC_TERM_WRAP termWrap;

  Rprintf("ebcache:\n");

  for(i=0;i<self->nRow;i++) {
    Rprintf("elt %d:\n",i);
    for(j=0;j<self->func->nbTermList;j++) {
      idTerm=self->func->termList[j];
      termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
      ebcache_vector_print(self->cache[i][j],termWrap->term->function->vectorLoc);
    }
  }
}

//for PseudoExpo use => give the matrix of non-trivial exhaustive stats!
//build the vector (normally a matrix) containing the cumulated value on one cache!
void ebcache_func_matrix(CACHE_FUNC self,DOUBLE* mat) {
  int i,j,k,idTerm;//,nCol;
  FUNC_TERM_WRAP termWrap;
  DOUBLE *tmp,*tmp2;

  //nCol= ebcache_func_ncol(self); 
  
  tmp=(DOUBLE*)Calloc(self->nCol,DOUBLE);
  
  for(i=0;i<self->nRow;i++) {
    tmp2=tmp;
    for(j=0;j<self->nCol;j++) tmp[j]=0.0; //init
    for(j=0;j<self->func->nbTermList;j++) {//printf("ici:%d,%d\n",i,j);
      idTerm=self->func->termList[j];
      termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
      ebcache_vector_updateInc(self->cache[i][j],termWrap->term->function->vectorLoc,tmp2,self->func->envR);
//printf("toto[%d,%d]=%LF\n",i,j,tmp2[0]);
      tmp2 += termWrap->term->function->vectorLoc->nbVector;
    }
    for(j=0;j<self->nCol;j++) mat[(self->nRow)*j+i]=tmp[j];
  }
  //to debug: for(i=0,k=0;i<self->nRow*self->nCol;i++) Rprintf("%LF ",mat[i]); Rprintf("\n");
  Free(tmp);
}

void ebcache_func_compute(CACHE_FUNC self,SEXP init, SEXP code) {
  int i,j,idTerm;
  FUNC_TERM_WRAP termWrap;

  //new sum vectors!
  /*for(j=0;j<self->func->nbTermList;j++) {
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    cqlsVector_initCache(termWrap->term->function->vectorCompFuncLoc);
  }*/

  //script initialization
  cqlsR_eval(self->func->envR,init);

  for(i=0;i<self->nRow;i++) {
    for(j=0;j<self->func->nbTermList;j++) {//printf("ici:%d,%d\n",i,j);
      idTerm=self->func->termList[j];
      termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
//printf("self->cache[i][j]=%d,vector=%d,vectorCompFunc=%d\n",self->cache[i][j],termWrap->term->function->vector,termWrap->term->function->vectorCompFunc);
      ebcache_vector_update(self->cache[i][j],termWrap->term->function->vectorLoc,termWrap->term->function->vectorCompFuncLoc,self->func->envR);
      //for debugging: just to watch!
      //printf("elt[%d,%d] ",i,j);cqlsVector_print_cache(termWrap->term->function->vectorCompFuncLoc);
    }
    //for debugging: just to watch!
    //cqlsVector_print_cache(termWrap->term->function->vectorCompFunc);
    //1) the results have to be put in the R system!
    cqlsVector_from_cache_to_envR(termWrap->term->function->vectorCompFuncLoc,self->func->envR);
    //2) the script is now to be evaluated!
    cqlsR_eval(self->func->envR,code);
  }
}


//IMPORTANT: no need to parse code since it is already parsed!
// This is an impressive function to compute approximation of integral and sum over the config! 
SEXP ebcache_func_sum_code(CACHE_FUNC self,SEXP code) {
  int i,j,idTerm,nb;
  FUNC_TERM_WRAP termWrap;
  SEXP tmpR,resR;

  //new sum vectors!
  /*for(j=0;j<self->func->nbTermList;j++) {
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    cqlsVector_initCache(termWrap->term->function->vectorCompFuncLoc);
  }*/

  //initialization i=0!
  for(j=0;j<self->func->nbTermList;j++) {//this is the first iteration 
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    ebcache_vector_update(self->cache[0][j],termWrap->term->function->vectorLoc,termWrap->term->function->vectorCompFuncLoc,self->func->envR);
  }
  cqlsVector_from_cache_to_envR(termWrap->term->function->vectorCompFuncLoc,self->func->envR);
  //EVERYTHING IN R!!!!
  PROTECT(resR= coerceVector(cqlsR_eval(self->func->envR,code),REALSXP));
  nb=length(resR);
  //Rprintf("long res code=%d\n",nb);
  //ebfunc_eval1("print(.V)");

  //The other step i!
  for(i=1;i<self->nRow;i++) {
    for(j=0;j<self->func->nbTermList;j++) {//printf("ici:%d,%d\n",i,j);
      idTerm=self->func->termList[j];
      termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
//printf("self->cache[i][j]=%d,vector=%d,vectorCompFunc=%d\n",self->cache[i][j],termWrap->term->function->vector,termWrap->term->function->vectorCompFunc);
      ebcache_vector_update(self->cache[i][j],termWrap->term->function->vectorLoc,termWrap->term->function->vectorCompFuncLoc,self->func->envR);
      //for debugging: just to watch!
      //printf("elt[%d,%d] ",i,j);cqlsVector_print_cache(termWrap->term->function->vectorCompFuncLoc);
    }
    //for debugging: just to watch!
    //cqlsVector_print_cache(termWrap->term->function->vectorCompFunc);
    //1) the results have to be put in the R system!
    cqlsVector_from_cache_to_envR(termWrap->term->function->vectorCompFuncLoc,self->func->envR);
    //2) the script is now to be evaluated!
    //ebfunc_eval1("print(.V)");
    PROTECT(tmpR=coerceVector(cqlsR_eval(self->func->envR,code),REALSXP));
    for(j=0;j<nb;j++) REAL(resR)[j] += REAL(tmpR)[j];
    UNPROTECT(1);
  }
 
  UNPROTECT(1);
  //Rprintf("long res code=%d\n",length(resR));
  return resR;
}

CACHE_FUNC_GLOB ebcache_funcGlob_new(FUNC func,PT_POLY poly) {
  CACHE_FUNC_GLOB self;

  self=(CACHE_FUNC_GLOB)Calloc(1,ST_CACHE_FUNC_GLOB);
  self->func=func;
  //for creation 
  ebcache_funcGlob_init(self,poly);
  return self;
}

void ebcache_funcGlob_free(CACHE_FUNC_GLOB self) {
  if(self!=(CACHE_FUNC_GLOB)NULL) {
    ebcache_vector_free((CACHE_VECTOR)self->cache);
    Free(self);          
  }
  self=(CACHE_FUNC_GLOB)NULL; 
}

void ebcache_funcGlob_init(CACHE_FUNC_GLOB self,PT_POLY poly) {//poly: no need as a field!
  int j,idTerm;
  FUNC_TERM_WRAP termWrap;

  //printf("np deb:%d\n",self->func);
  //by assuming the point is tried outside
  //Rmk: below 0 stands for a global cache
  ebfunc_componentCacheNew(self->func,-1); //printf("np new\n");//all the ebfunc_cache_vector are created!
  ebfunc_componentCacheUpdate(self->func,poly); //printf("np updated\n");//now updated!
  //
  self->cache=(CACHE_VECTOR*)Calloc(1,CACHE_VECTOR);//printf("np allocated\n");
  for(j=0;j<self->func->nbTermList;j++) {
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    self->cache[j]=ebcache_vector_new(termWrap->term->function->cacheGlob);
    //printf("self->cache[%d][%d]:\n",self->cptRow,j);
    //ebcache_vector_print(self->cache[self->cptRow][j],termWrap->term->function->vector);
  }
  //printf("np caches vector new\n");
  ebfunc_componentCacheFree(self->func);//printf("np freed\n"); //free all the ebfunc_cache_vector
}

void ebcache_funcGlob_print(CACHE_FUNC self) {
  int i,j,idTerm;
  FUNC_TERM_WRAP termWrap;

  Rprintf("ebcache (global):\n");

  for(j=0;j<self->func->nbTermList;j++) {
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    ebcache_vector_print((CACHE_VECTOR)self->cache,termWrap->term->function->vectorGlob);
  }
}

//no compute, only update because no script seems to ne necessary!
void ebcache_funcGlob_update(CACHE_FUNC self) {
  int i,j,idTerm;
  FUNC_TERM_WRAP termWrap;

  for(j=0;j<self->func->nbTermList;j++) {//printf("ici:%d,%d\n",i,j);
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    ebcache_vector_update((CACHE_VECTOR)self->cache,termWrap->term->function->vectorGlob,termWrap->term->function->vectorCompFuncGlob,self->func->envR);
  }
  //for debugging: just to watch!
  //cqlsVector_print_cache(termWrap->term->function->vectorCompFuncGlob);
  //1) the results have to be put in the R system!
  cqlsVector_from_cache_to_envR(termWrap->term->function->vectorCompFuncGlob,self->func->envR);
} 

