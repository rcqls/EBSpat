#include "ebcache.h"
#include "ebfunc.h"
#include "ebcalcul.h"

//CACHE WEIGHTED VECTOR
CACHE_W_VECTOR ebcache_wVector_new(FUNC func) {
  CACHE_W_VECTOR self;
  int j,idTerm;
  FUNC_TERM_WRAP termWrap;

  self=(CACHE_W_VECTOR)Calloc(1,ST_CACHE_WEIGHTED_VECTOR);Pb_M(self,0);
  self->func=func;
  self->vector=(CQLS_VECTOR*)Calloc(self->func->nbTermList,CQLS_VECTOR);Pb_M(self->vector,1);
  //self->nbVector=(int*)Calloc(self->func->nbTermList,int);
  self->nbVector=0;
  //to easily get all the vectors (CQLS_VECTOR)!
  for(j=0;j<self->func->nbTermList;j++) {
    idTerm=self->func->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->func->terms[idTerm]);
    //Rprintf("j=%d,idTerm=%d,termWrap=%p\n",j,idTerm,termWrap);
    //Rprintf("vectorCompFunc=%p\n",termWrap->term->function->vectorCompFuncLoc);
    self->vector[j]=termWrap->term->function->vectorCompFuncLoc;
    //self->nbVector[j]=termWrap->term->function->vectorCompFuncLoc->nbVector;
    self->nbVector += self->vector[j]->nbVector;
  }

  self->nbCache=0;
  self->cache=(DOUBLE**)NULL;
  self->weight=(int*)NULL;
  return self;
}


void ebcache_wVector_empty_cache(CACHE_W_VECTOR self) {
  int i;

  if(self->cache!=(DOUBLE**)NULL)  {
    for(i=0;i<self->nbCache;i++) Free(self->cache[i]);
    Free(self->cache);
    self->cache=(DOUBLE**)NULL;
    self->nbCache=0;      
    if(self->weight!=(int*)NULL) {
      Free(self->weight);
      self->weight=(int*)NULL;
    }
  }
}


void ebcache_wVector_free(CACHE_W_VECTOR self) {
  int i;

  if(self!=(CACHE_W_VECTOR)NULL) {
    Free(self->vector);//only the pointer array not their contents. 
    ebcache_wVector_empty_cache(self);
    Free(self);
    self=(CACHE_W_VECTOR)NULL;
  }
}

void ebcache_wVector_realloc(CACHE_W_VECTOR self,int nb) {
  self->nbCache += nb;
  //Rprintf("Alloc nbCache=%d(+%d)\n",self->nbCache,nb);
  self->cache=(DOUBLE**)Realloc(self->cache,self->nbCache,DOUBLE*);Pb_M(self->cache,2);
  self->cache[self->nbCache -1]=(DOUBLE*)NULL;
  self->weight=(int*)Realloc(self->weight,self->nbCache,int);Pb_M(self->weight,3);
  self->weight[self->nbCache -1]=(int)0;
  //Rprintf("done\n");
}

int ebcache_wVector_index_for_vector_in_cache(CACHE_W_VECTOR self,DOUBLE *vector) {
  int i,j,idTerm,cpt;
  FUNC_TERM_WRAP termWrap;
  DOUBLE *tmp,*tmp2;  

  for(i=0;i<self->nbCache;i++) {
    tmp=vector;tmp2=self->cache[i];
    cpt=0;
    for(j=0;j<self->func->nbTermList;j++) {
      //compare the element of vector with cache element!
      if(cqlsVector_same_vectors(self->vector[j],tmp,tmp2)) cpt++;
      else break;
      tmp += self->vector[j]->nbVector;tmp2 += self->vector[j]->nbVector;
    }
    if(cpt==self->func->nbTermList) break; //found in the cache!
  }
  return i; //not found in the cache if i==self->nbCache!
}

DOUBLE* ebcache_wVector_copy_ansCache(CACHE_W_VECTOR self) {
  int j;
  DOUBLE *vector,*tmp;
  
  vector=(DOUBLE*)Calloc(self->nbVector,DOUBLE);Pb_M(vector,4);//Rprintf("vector=%p\n",vector);
  tmp=vector;//Rprintf("tmp=%p\n",tmp);
  for(j=0;j<self->func->nbTermList;j++) {
    //Rprintf("self->vector[%d]=%p\n",j,self->vector[j]);
    cqlsVector_initVector_fromCache(self->vector[j],tmp);
    tmp += self->vector[j]->nbVector;
  }
  return vector;
}

int ebcache_wVector_add_vector_in_cache(CACHE_W_VECTOR self) {
  int i;
  DOUBLE* vector;

  vector=ebcache_wVector_copy_ansCache(self);//Rprintf("vector=%p\n",vector);
  i=ebcache_wVector_index_for_vector_in_cache(self,vector);
  //Rprintf("index=%d,self->nbCache=%d\n",i,self->nbCache);
  if(i<self->nbCache) {
    //Rprintf("to free %p",vector);
    Free(vector);
    //Rprintf("-> done\n");
    (self->weight[i])++;
  } else {//append the vector!
    ebcache_wVector_realloc(self,(int)1);
    //cqlsVector_print_cache(self->vector);
    self->cache[self->nbCache-1]=vector;
    //cqlsVector_print(self->vector,self->cache[self->nbCache-1]);
    self->weight[self->nbCache-1]=1;
  }  
}

void ebcache_wVector_print(CACHE_W_VECTOR self) {
  int i,j;

  //cqlsVector_show(self->vector);
  for(i=0;i<self->nbCache;i++) {
    Rprintf("weight=%d",self->weight[i]);
    for(j=0;j<self->nbVector;j++) Rprintf(",v[%d]=%LF ",j,self->cache[i][j]);
    Rprintf("\n");
  }
}

SEXP ebcache_wVector_weight_to_R_as_vector(CACHE_W_VECTOR self) {
  SEXP weightR;
  int i;

  PROTECT(weightR=allocVector(INTSXP,self->nbCache));
  for(i=0;i<self->nbCache;i++) INTEGER(weightR)[i]=self->weight[i];
  UNPROTECT(1);
  return weightR;
}

/*
SEXP ebcache_wVector_to_R_as_dataframe(CACHE_W_VECTOR self) {
  SEXP resR;
  int j;
  DOUBLE *tmp;

  for(i=0,ii=0;i<self->nbBlockVector;i++) {
      PROTECT(tmpR=allocVector(REALSXP,self->sizeBlockVector[i]));
      for(j=0;j<self->sizeBlockVector[i];j++,ii++) REAL(tmpR)[j]=vector[ii];
      SET_VECTOR_ELT(resR,i,tmpR);
      UNPROTECT(1);
      SET_STRING_ELT(nameR,i,mkChar(self->keyBlockVector[i]));
    }

  //solution; create list of dataframe and bind them in R!
  PROTECT(resR=allocVector(VECSXP,self->func->nbTermList));
  tmp=self->cache;
  for(j=0;j<self->func->nbTermList;j++) {
    SET_VECTOR_ELT(resR,j,cqlsVector_vector_array_to_R_as_dataframe(self->vector[j],tmp,self->nbCache)
  setAttrib(resR,install("weight"),ebcache_wVector_weight_to_R_as_vector(self));
  UNPROTECT(1);
  return resR;
}*/

SEXP ebcache_wVector_Rnames(CACHE_W_VECTOR self) {
  int i,j,ii,k;
  SEXP names;

  PROTECT(names=allocVector(STRSXP,self->nbVector+1));
  SET_STRING_ELT(names,0,mkChar("weight"));
  for(k=0,ii=1;k<self->func->nbTermList;k++) {
     //Rprintf("k=%d\n",k);
    for(i=0;i<self->vector[k]->nbBlockVector;i++) for(j=0;j<self->vector[k]->sizeBlockVector[i];j++,ii++) {
      //Rprintf("key[%d]=%s\n",ii,self->vector[k]->keyBlockVector[i]);
      SET_STRING_ELT(names,ii,mkChar(self->vector[k]->keyBlockVector[i]));
    }  
  }  
  UNPROTECT(1);
  return names;
}

SEXP ebcache_wVector_to_R_as_dataframe(CACHE_W_VECTOR self) {
  int i,j;
  SEXP resR,*tmpR,tmpR2,rownameR;  

//Rprintf("cqlsVector_vector_array_to_R_as_dataframe\n");
  PROTECT(resR=allocVector(VECSXP,self->nbVector+1));//resR<-list()
  tmpR=(SEXP*)Calloc(self->nbVector,SEXP);
  PROTECT(rownameR = allocVector(INTSXP, self->nbCache));
  for(j=0;j<self->nbVector;j++) PROTECT(tmpR[j]=allocVector(REALSXP,self->nbCache));

  for(i=0;i<self->nbCache;i++) {    
    for(j=0;j<self->nbVector;j++) { 
      REAL(tmpR[j])[i] = self->cache[i][j];
      //Rprintf("i,j=%d,%d -> %LF,%lf\n",i,j,self->cache[i][j],REAL(tmpR[j])[i]);  
    }
    INTEGER(rownameR)[i] = i+1;
  }

  SET_VECTOR_ELT(resR,0,ebcache_wVector_weight_to_R_as_vector(self));
  for(j=0;j<self->nbVector;j++) SET_VECTOR_ELT(resR,j+1,tmpR[j]);

  setAttrib(resR, R_NamesSymbol,ebcache_wVector_Rnames(self));
 
  PROTECT(tmpR2 = allocVector(STRSXP, 1));
  SET_STRING_ELT(tmpR2, 0, mkChar("data.frame"));
  classgets(resR, tmpR2);
  setAttrib(resR, R_RowNamesSymbol, rownameR);
  UNPROTECT(1);
  Free(tmpR);
  UNPROTECT(2 + self->nbVector);
  return resR;
}


//CACHE COMP FUNC
CACHE_COMP_FUNC ebcache_compFunc_new(FUNC func) {
  CACHE_COMP_FUNC self;
  PT_LIST *pt;
  int i,j,idTerm;
  FUNC_TERM_WRAP termWrap;

  self=(CACHE_COMP_FUNC)Calloc(1,ST_CACHE_COMP_FUNC);
  self->func=func;
  self->cache=ebcache_wVector_new(self->func);
  //Rprintf("done\n");
  return self;
}

void ebcache_compFunc_free(CACHE_COMP_FUNC self) {
int i;

  if(self!=(CACHE_COMP_FUNC)NULL) {
    //empty all the stuff!
    ebcache_wVector_free(self->cache);
    Free(self);
    self=(CACHE_COMP_FUNC)NULL;
  }
} 

void ebcache_compFunc_empty_cache(CACHE_COMP_FUNC self) {

    ebcache_wVector_empty_cache(self->cache);//empty but still alive!

}

void ebcache_compFunc_print(CACHE_COMP_FUNC self) {

    ebcache_wVector_print(self->cache);
}

void ebcache_compFunc_new_dv(CACHE_COMP_FUNC self,PT_POLY poly) {//poly: no need as a field!
  int i,idTerm;
  FUNC_TERM_WRAP termWrap;

  //printf("np deb:%d\n",self->func);
  //by assuming the point is tried outside
  //Rmk: below 1 stands for a local cache
  ebfunc_componentCacheNew(self->func,1); // Rprintf("np new\n");//all the ebfunc_cache_vector are created!
  ebfunc_componentCacheUpdate(self->func,poly); //Rprintf("np updated\n");//now updated!
  ebcache_wVector_add_vector_in_cache(self->cache);
  ebfunc_componentCacheFree(self->func);//Rprintf("np freed\n"); //free all the ebfunc_cache_vector
 
}

void ebcache_compFunc_init_systSampling(CACHE_COMP_FUNC self,int nbGridPts,DOUBLE *domainSize,PT_POLY poly) {
  unsigned int i,nb,nbPrint;
  DOUBLE x,y,pas[2];
  PT_DV pt_dv;

  ebcache_compFunc_empty_cache(self);
  nb=(unsigned int)nbGridPts*nbGridPts;
  nbPrint=(unsigned int)nb/1000;
  if(nbPrint<1) nbPrint=1;
  pas[0]=domainSize[0]/(DOUBLE)(nbGridPts);pas[1]=domainSize[1]/(DOUBLE)(nbGridPts);
  Rprintf("nb=%d,domainSize=(%LF,%LF)\n",nb,domainSize[0],domainSize[1]);
  //TODO:  debug since (x,y) have to the center of the square!!!
  for(x=poly->vg->center[0]-domainSize[0]/2.0+pas[0]/2.0,i=0;x<=poly->vg->center[0]+domainSize[0]/2.0;x+=pas[0]) 
  for(y=poly->vg->center[1]-domainSize[1]/2.0+pas[1]/2.0;y<=poly->vg->center[1]+domainSize[1]/2.0;y+=pas[1],i++) {
    if(i%nbPrint==0) Rprintf("\ri=%d,x=%LF,y=%LF ",i,x,y);
    pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(poly->vg);Pb_M(pt_dv,10);
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
    ebcache_compFunc_new_dv(self,poly);//printf("new cache point done\n");
    ebpoly_final_ins(poly);//IMPORTANT: the point is not inserted. Cancel is done in the calculation of interactions in EBFunc!
  }
  Rprintf("\n");
}

void ebcache_compFunc_init_randSampling(CACHE_COMP_FUNC self,int nb,DOUBLE *domainSize,PT_POLY poly) {
  unsigned int i,nbPrint;
  PT_DV pt_dv;

   
  ebcache_compFunc_empty_cache(self);
  Rprintf("nb=%d,domainSize=(%LF,%LF)\n",nb,domainSize[0],domainSize[1]);
  nbPrint=(unsigned int)nb/1000;
  GetRNGstate();
  for(i=0;i<nb;i++) {
    pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(poly->vg);Pb_M(pt_dv,10);
    (*pt_dv)->point[0]=(DOUBLE)(unif_rand()*domainSize[0] - domainSize[0]/2.0 + poly->vg->center[0]);
    (*pt_dv)->point[1]=(DOUBLE)(unif_rand()*domainSize[1] - domainSize[1]/2.0 + poly->vg->center[1]);
    //Check: if(i%nbPrint==0) Rprintf("\rpt_dv[%d]=(%LF,%LF)",i,(*pt_dv)->point[0],(*pt_dv)->point[1]);
    //marks generation
    if(poly->vg->marks->nbData) {//there is marks!
      DC_curObj(poly->vg->marks->dc,*pt_dv);
      //TODO: TO FIX! 
      DU_cqlsSEXP_getAllVars_List(cqlsRExpr_eval(poly->vg->marksGen),poly->vg->marks);
    }
    //printf("make ins-> ?\n");
    ebpoly_make_ins(poly,pt_dv); //printf("done! new cache point ?\n");
    ebcache_compFunc_new_dv(self,poly);//printf("new cache point done\n");
    ebpoly_final_ins(poly);//IMPORTANT: the point is not inserted. Cancel is done in the calculation of interactions in EBFunc!  
  }
  Rprintf("\n");
  PutRNGstate();
}



void ebcache_compFunc_init_dvsSum(CACHE_COMP_FUNC self,DOUBLE *domainSize,PT_POLY poly) {
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

  ebcache_compFunc_empty_cache(self);
  nb=ebpoly_nbPoints_inside(poly,domainSize);

  for(i=0;i<poly->vg->nb_dv;i++) {
    pt_dv=poly->vg->tab_dv+i;
    ///printf("point=%d\n",*pt_dv);
    if(*pt_dv==(DV)NULL) continue;
    //if((*pt_dv)->num_s==-3) continue;
    if(!ebdv_dans_domaine(pt_dv,poly->vg->center,domainSize)) continue;
    ebpoly_make_sup(poly,pt_dv);
    ebcache_compFunc_new_dv(self,poly);
    ebpoly_final_sup(poly);//IMPORTANT: the point is not deleted. Cancel is done in the calculation of interactions in EBFunc!
  }
}


SEXP ebcache_compFunc_to_R_as_dataframe(CACHE_COMP_FUNC self) {

  return ebcache_wVector_to_R_as_dataframe(self->cache);
}



