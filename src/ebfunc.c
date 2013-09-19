#include <stdio.h>
#include <stdlib.h>
#include "ebvor.h"
#include "ebcalcul.h"
#include "eblist.h"
#include <string.h>
#include "ebfunc.h"

#define debugModeR_OFF

#define DEBUG_PARAMzz
#define DEBUG_COMPONENT_ELzz
#define DEBUG_PSEUDO_STRUCTzz
#define DEBUG_PSEUDO_COMPONENTUPDATEzz
#define DEBUG_PSEUDO_COMPONENT_EDGEzzz
#define VERBOSE_NEWzz
///////////////////////////////////////////////////////////// FUNC
/***************/
/*    Func    */
/***************/
FUNC ebfunc_new() {
  FUNC self;
  int i;

  self=(FUNC)Calloc(1,ST_FUNC);
  self->nbTerms=FUNC_TERM_SIZE; //predefined
  self->terms=(FUNC_TERM_WRAP*)Calloc(FUNC_TERM_SIZE,FUNC_TERM_WRAP);
  for(i=0;i<self->nbTerms;i++) {
    self->terms[i]=(FUNC_TERM_WRAP)NULL;
  }
  self->termList=NULL;
  self->nbTermList=0;
  self->termTypeList=NULL; //to remember the real type
  //self->single=0;
  //self->param=NULL;
  self->nbComponent=0;
  self->component=NULL;
  self->kind=0;
  //self->modpar-> allocated when update_funcParam is called!
  ebfunc_init();
  self->envR=EBFUNC_R;
#ifdef VERBOSE_NEW
printf("ebfunc_new fin!\n");
printf("nbTermList=%d\n",self->nbTermList);
#endif
  return self;
}

void ebfunc_free(FUNC self) {
  int i;
  FUNC_TERM_WRAP termWrap;

  if (self!=(FUNC)NULL) {
    //vider la list des terms!
    for(i=0;i<self->nbTerms;i++) {
      //Rprintf("i=%d\n",i);
      termWrap=(FUNC_TERM_WRAP)(self->terms[i]);
      //Rprintf("termWrap=%p\n",termWrap);
      if(termWrap!=NULL) {
        (termWrap->free)(termWrap->term);
        Free(termWrap);
      }
    }
    Free(self->terms);
    Free(self->termList);
    Free(self->termTypeList);
    //ebfunc_param_free(self->modpar);
    Free(self);
  }
}

//first only one func env !!!
void ebfunc_init() {
  if(EBFUNC_R==NULL) {
    CQLSR_start();
    EBFUNC_R=cqlsR_new();
    cqlsR_setEnv(EBFUNC_R,EBFuncEnvName);
  }
}

//compute the number of columns!
int ebfunc_ncol(FUNC self) {
  int j,idTerm,nCol=0;
  FUNC_TERM_WRAP termWrap;   
  
  for(j=0;j<self->nbTermList;j++) {
    idTerm=self->termList[j];
    termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
    nCol += termWrap->term->function->vectorLoc->nbVector;
  }
  //Rprintf("matrix col=%d\n",nCol);
  return nCol;
}

SEXP ebfunc_eval1(char *cmdString) {
  return cqlsR_eval1(EBFUNC_R,cmdString);
}

//
/*void ebfunc_add_single(FUNC self) {
  if(self->single==0) {
    self->single=1;
  }
}

void ebfunc_init_single(DOUBLE single) {
  //char cmd[20];

  sprintf(cmd,"%s<-%LF",FUNC_SINGLE_KEY,single);
  ebfunc_eval1(cmd);
}*/


//TODO: think about inserting more than one Del2 depending on other parameter (like the Delaunay order)
// -> declare new element in self->terms with a new id.
// -> maybe in R, save all the declared id depending on the type of interaction or just let the user the ability to declare a new id associated to an idTerm.
// ex: Del2 and order=2
// idTerm=Del2 and order=2 will be obtained from the new id!  
//NEW: user defined function can be add at the end with an id corresponding to the last
void ebfunc_add_function(FUNC self,FUNCTION function,int newIdTermUserDefined) {
  FUNC_TERM_WRAP termWrap;
  int termType,idTerm; //idTerm is a unique identifier when termType is the type of the term.

  //for predefined terms, both idTerm and termType match.
  idTerm=termType=function->id;
#ifdef debugModeR
    Rprintf("idTerm=type=%d\n",idTerm);
#endif
  if(newIdTermUserDefined) {//for user-defined term, idTerm is appended
    idTerm=self->nbTerms;
#ifdef debugModeR
    Rprintf("New user-defined idTerm=%d of type %d\n",idTerm,termType);
#endif
    self->terms=(FUNC_TERM_WRAP*)Realloc(self->terms,++(self->nbTerms),FUNC_TERM_WRAP);
    self->terms[idTerm]=(FUNC_TERM_WRAP)NULL;//VERY IMPORTANT because of the next test! (I forgot first and the rest was undeclared!)
  }
  //declaration of function[idTerm] if it does not exists (as generally it does not)!
  //DEBUG: Normally nil -> Rprintf("self->terms[idTerm=%d]=%p of type %d\n",idTerm,self->terms[idTerm],termType);
  if((self->terms[idTerm])==(FUNC_TERM_WRAP)NULL) {
    termWrap=self->terms[idTerm]=(FUNC_TERM_WRAP)Calloc(1,ST_FUNC_TERM_WRAP);
    self->nbTermList++;
    self->termList=(int*)Realloc(self->termList,self->nbTermList,int);
    self->termList[self->nbTermList-1]=idTerm;
    self->termTypeList=(int*)Realloc(self->termTypeList,self->nbTermList,int);
    self->termTypeList[self->nbTermList-1]=termType;
    switch(termType) {
      case FUNC_TYPE_DEL1:
#ifdef debugModeR
        Rprintf("Del1 term\n");
#endif
	      termWrap->term=(FUNC_TERM_)ebfunc_termDel1_new(function);
	      termWrap->free=ebfunc_termDel1_free;
	      termWrap->localEnergy=ebfunc_termDel1_localEnergy;
        termWrap->globalEnergy=ebfunc_termDel1_globalEnergy;
	      termWrap->componentLocalUpdate=ebfunc_termDel1_componentLocalUpdate;
        termWrap->componentGlobalUpdate=ebfunc_termDel1_componentGlobalUpdate;
	      break;
      case FUNC_TYPE_DEL2:
#ifdef debugModeR
        Rprintf("Del2 term\n");
#endif
        termWrap->term=(FUNC_TERM_)ebfunc_termDel2_new(function);
        termWrap->free=ebfunc_termDel2_free;
        termWrap->localEnergy=ebfunc_termDel2_localEnergy;
        termWrap->globalEnergy=ebfunc_termDel2_globalEnergy;
        termWrap->componentLocalUpdate=ebfunc_termDel2_componentLocalUpdate;
        termWrap->componentGlobalUpdate=ebfunc_termDel2_componentGlobalUpdate;
        break;
      case FUNC_TYPE_DEL3:
#ifdef debugModeR
        printf("Del3 term\n");
#endif
	      termWrap->term=(FUNC_TERM_)ebfunc_termDel3_new(function);
	      termWrap->free=ebfunc_termDel3_free;
	      termWrap->localEnergy=ebfunc_termDel3_localEnergy;
        termWrap->globalEnergy=ebfunc_termDel3_globalEnergy;
	      termWrap->componentLocalUpdate=ebfunc_termDel3_componentLocalUpdate;
        termWrap->componentGlobalUpdate=ebfunc_termDel3_componentGlobalUpdate;
	      break;
      case FUNC_TYPE_ALL2:
#ifdef debugModeR
        printf("All2 term\n");
#endif
	      termWrap->term=(FUNC_TERM_)ebfunc_termAll2_new(function);
	      termWrap->free=ebfunc_termAll2_free;
	      termWrap->localEnergy=ebfunc_termAll2_localEnergy;
        termWrap->globalEnergy=ebfunc_termAll2_globalEnergy;
	      termWrap->componentLocalUpdate=ebfunc_termAll2_componentLocalUpdate;
        termWrap->componentGlobalUpdate=ebfunc_termAll2_componentGlobalUpdate;
        break;
      case FUNC_TYPE_NNG:
#ifdef debugModeR
        printf("NNG term\n");
#endif
	      termWrap->term=(FUNC_TERM_)ebfunc_termNNG_new(function);
	      termWrap->free=ebfunc_termNNG_free;
	      termWrap->localEnergy=ebfunc_termNNG_localEnergy;
        termWrap->globalEnergy=ebfunc_termNNG_globalEnergy;
	      termWrap->componentLocalUpdate=ebfunc_termNNG_componentLocalUpdate;
        termWrap->componentGlobalUpdate=ebfunc_termNNG_componentGlobalUpdate;
        break;
    }
  } else termWrap=self->terms[idTerm];
  //show list
  //ebfunc_show_functionList(self);
#ifdef debugModeR
  Rprintf("Check: self->terms[idTerm=%d]=%p of type %d\n",idTerm,(void*)termWrap,termType);
#endif
}

void ebfunc_show_functionList(FUNC self) {
  FUNC_TERM_ term;
  int i,idTerm,j;
  char opt[100];
  
  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    term=(FUNC_TERM_)(self->terms[idTerm]->term);
#ifdef debugModeR
    Rprintf("self->terms[idTerm]:%d\n",(int)self->terms[idTerm]);
#endif
    if ( idTerm == self->termTypeList[i]) sprintf(opt,""); 
    else 
      switch(self->termTypeList[i]) {
        case FUNC_TYPE_DEL1: 
          sprintf(opt,"");
          break;
        case FUNC_TYPE_DEL2:  
          sprintf(opt,", order=%d",((FUNC_TERM_DEL2)term)->order);
          break;
        case FUNC_TYPE_NNG:
          sprintf(opt,", order=%d",((FUNC_TERM_NNG)term)->order);
          break;
        case FUNC_TYPE_ALL2:  
          sprintf(opt,", range=%LF",((FUNC_TERM_ALL2)term)->range);
          break;
      }
    Rprintf("show term: id=%d, type=%d%s\n",idTerm,self->termTypeList[i],opt);
    ebfunc_function_show(term->function);
  }
}
 
void ebfunc_componentInit(FUNC self) {
  int i=0;
  for(i=0;i<self->nbComponent;i++) self->component[i]=(DOUBLE)0.0;
}

////SEULE INTERFACE AU NIVEAU ENERGIE!!!
DOUBLE ebfunc_localEnergy(FUNC self, PT_POLY poly) {
  DOUBLE V_x_phi;
  int i,idTerm;
  FUNC_TERM_WRAP termWrap;
 
  //printf("energy\n");
  //single first ->no need component here!!!
  V_x_phi = (DOUBLE)ebfunc_get_single(); //param[0]; //or read single in R!!!
  
  //printf("single=%LF\n",self->param[0]);
  //other terms (non expo here)
  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    //printf("idTerm=%d\n",idTerm);
    termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
    V_x_phi += (termWrap->localEnergy)(termWrap->term,poly);
  }
  return V_x_phi;
}

////SEULE INTERFACE AU NIVEAU ENERGIE!!!
DOUBLE ebfunc_globalEnergy(FUNC self, PT_POLY poly) {
  DOUBLE V_x_phi;
  int i,idTerm;
  FUNC_TERM_WRAP termWrap;
 
  //printf("energy\n");
  //single first ->no need component here!!!
  V_x_phi = (DOUBLE)ebfunc_get_single() * ebpoly_nb_dvs(poly,0) ;//(DOUBLE)ebfunc_get_single(); //param[0]; //or read single in R!!!
  
  //printf("single=%LF\n",self->param[0]);
  //other terms (non expo here)
  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
#ifdef debugModeR
    Rprintf("idTerm=%d\n",idTerm);
#endif
    termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
    V_x_phi += (termWrap->globalEnergy)(termWrap->term,poly);
  }
  return V_x_phi;
}



///////////////////////////////////////////////////////////// FUNCTION

//FUNC INFO
void ebfunc_info_free(FUNC_INFO self) {
  if (self!=(FUNC_INFO)NULL) {
    Free(self->infos);//TODO: Free its elements ???
    Free(self->infoList);
    Free(self);
  }
}

FUNC_INFO ebfunc_info_new(int nbInfos) {
  FUNC_INFO self;
  int i;

  self=(FUNC_INFO)Calloc(1,ST_FUNC_INFO);
  self->infos=(int*)Calloc(nbInfos,int);
  for(i=0;i<nbInfos;i++) self->infos[i]=0;
  self->nbInfoList=0;
  self->infoList=NULL;
  return self;
}

void ebfunc_info_add(FUNC_INFO self, int *infoList, int nbInfoList,int from) {
  int startInfo=self->nbInfoList,i,ii;

  ii=startInfo;
  for(i=from;i<nbInfoList;i++) {
    if(!(self->infos[infoList[i]])) {//only if it does not exist yet
      (self->nbInfoList)++;
      self->infoList=(int*)Realloc(self->infoList,self->nbInfoList,int);
      self->infoList[ii++]=infoList[i];
    }
    (self->infos[infoList[i]])++;
  }
}

//to access nbParam of term
//int ebfunc_termWrap_nbParam(FUNC_TERM term) {
//  return ((FUNC_TERM_FUNCTION)(term->function))->nbParam;
//}

///////////////////////////////////////////////////////////// FUNC COMPO
//TODO: the same but by specifying the idTerm
void ebfunc_componentCacheNew(FUNC self,int local) {//local=-1 (global), 0 (both) ,1 (local)
  FUNC_TERM_WRAP termWrap;
  int i,idTerm;

  self->local=local;
  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
    /*printf("termWrap->term:%d\n",termWrap->term);
    Rprintf("termWrap->term->function:%d\n",termWrap->term->function);
    Rprintf("termWrap->term->function->vector:%d\n",termWrap->term->function->vector);
    Rprintf("termWrap->term->function->cache:%d\n",termWrap->term->function->cache);*/
    if(self->local >= 0) {
      if(termWrap->term->function->vectorLoc!=(CQLS_VECTOR)NULL)
        termWrap->term->function->cacheLoc=ebfunc_vector_cache_new(termWrap->term->function->vectorLoc);
    } 
    if(self->local <= 0) {
      if(termWrap->term->function->vectorGlob!=(CQLS_VECTOR)NULL)
        termWrap->term->function->cacheGlob=ebfunc_vector_cache_new(termWrap->term->function->vectorGlob);
    }
  }
}

void ebfunc_componentCacheFree(FUNC self) {
  FUNC_TERM_WRAP termWrap;
  int i,idTerm;

  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
    if(self->local>=0) {
    if(termWrap->term->function->vectorLoc!=(CQLS_VECTOR)NULL)
      ebfunc_vector_cache_free(termWrap->term->function->cacheLoc);
    }
    if(self->local<=0) {
    if(termWrap->term->function->vectorGlob!=(CQLS_VECTOR)NULL)
      ebfunc_vector_cache_free(termWrap->term->function->cacheGlob);
    }
  }
}


void ebfunc_componentCacheUpdate(FUNC self,PT_POLY poly) {
  FUNC_TERM_WRAP termWrap;
  int i,idTerm;

  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
//printf("CacheUpdate: idTerm=%d\n",idTerm);
    termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
    if(self->local>=0) {
      (termWrap->componentLocalUpdate)(termWrap->term,poly);
      ebfunc_vector_cache_cumVector(termWrap->term->function->cacheLoc,termWrap->term->function->vectorCompFuncLoc,self->envR); 
    }
    if(self->local<=0) {
//Rprintf("self->local<0\n");
      (termWrap->componentGlobalUpdate)(termWrap->term,poly);
      ebfunc_vector_cache_cumVector(termWrap->term->function->cacheGlob,termWrap->term->function->vectorCompFuncGlob,self->envR); 
    }
  }
}

void ebfunc_componentCacheShow(FUNC self,int funcId) {
  FUNC_TERM_WRAP termWrap;
  int i,idTerm;
  
  
  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    if(funcId<0 || idTerm==funcId) {
      termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
      if(self->local>=0) ebfunc_vector_cache_show(termWrap->term->function->cacheLoc);
      if(self->local<=0) ebfunc_vector_cache_show(termWrap->term->function->cacheGlob);
    }
  }
}

//Do not move local, since local is used for get and self->local for update!
SEXP ebfunc_componentCacheGet(FUNC self,int funcId,int local) {
  FUNC_TERM_WRAP termWrap;
  int i,idTerm;
  
  
  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    if(funcId<0 || idTerm==funcId) {
//Rprintf("idTerm=%d",idTerm);
      termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
      if(local>0) {
        return ebfunc_vector_cache_lists_to_R_as_named_list(termWrap->term->function->cacheLoc);
      } else {
//Rprintf("CacheGet: global\n");
        return ebfunc_vector_cache_lists_to_R_as_named_list(termWrap->term->function->cacheGlob);
      }
    }
  }
}

SEXP ebfunc_componentCacheGetFunc(FUNC self,int funcId,int local) {
  FUNC_TERM_WRAP termWrap;
  int i,idTerm;
  
  
  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    if(funcId<0 || idTerm==funcId) {
//Rprintf("idTerm=%d\n",idTerm);
      termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
      if(local>0) {
        return cqlsVector_cache_to_R_as_named_list(termWrap->term->function->vectorCompFuncLoc);
        //No more neeed to update CompFunc since this is done in CacheUpdate!
        //Obsolete: ebfunc_vector_cache_cumVector_to_R_as_named_list_from_vector(termWrap->term->function->cacheLoc,termWrap->term->function->vectorCompFuncLoc,self->envR);
      } else {
        return cqlsVector_cache_to_R_as_named_list(termWrap->term->function->vectorCompFuncGlob);
        //No more neeed to update CompFunc since this is done in CacheUpdate!
        //Obsolete: ebfunc_vector_cache_cumVector_to_R_as_named_list_from_vector(termWrap->term->function->cacheGlob,termWrap->term->function->vectorCompFuncGlob,self->envR);
      }
    }
  }
}

/*void ebfunc_componentCacheExport(FUNC self,int funcId) {
  FUNC_TERM_WRAP termWrap;
  int i,idTerm;

  for(i=0;i<self->nbTermList;i++) {
    idTerm=self->termList[i];
    if(funcId<0 || idTerm==funcId) {
      termWrap=(FUNC_TERM_WRAP)(self->terms[idTerm]);
      ebfunc_vector_cache_show(termWrap->term->function->cache);
    }
  }
}*/

//FUNC_VECTOR_CACHE

FUNC_VECTOR_CACHE ebfunc_vector_cache_new(CQLS_VECTOR vector) {
  FUNC_VECTOR_CACHE self;
  
  self=(FUNC_VECTOR_CACHE)Calloc(1,ST_FUNC_VECTOR_CACHE);Pb_M(self,100);
  self->oldList=cqlsVectorList_new(vector);
  self->list=cqlsVectorList_new(vector);
  return self;
}

void ebfunc_vector_cache_free(FUNC_VECTOR_CACHE self) {
  cqlsVectorList_free(self->oldList);
  cqlsVectorList_free(self->list);
  Free(self);
}

void ebfunc_vector_cache_show(FUNC_VECTOR_CACHE self) {
  if(self!=(FUNC_VECTOR_CACHE)NULL) {
    if(self->list==(CQLS_VECTOR_LIST)NULL) {
      Rprintf("list is empty!\n");
    } else {
      Rprintf("show list:\n");
      cqlsVectorList_print(self->list);
    }
    if(self->oldList==(CQLS_VECTOR_LIST)NULL) {
      Rprintf("oldList is empty!\n");
    } else {
      Rprintf("show oldList:\n");
      cqlsVectorList_print(self->oldList);
    }
  } else {
    Rprintf("vector cache is empty!\n");
  }
}

//after calling this function, cumVector contains in its ansCache its cumulated values
//after an update of self->list->vector then self->oldList->vector.
void ebfunc_vector_cache_cumVector(FUNC_VECTOR_CACHE self,CQLS_VECTOR cumVector,CQLS_R envR) {
PT_LIST pt;
DOUBLE *vector;

  if(cumVector->nbBlockVector==0) return;

  cqlsVector_initCache(cumVector);
  for(pt=self->list->list;pt!=NULL;pt=pt->pt_next) {
    vector=(DOUBLE*)pt->pt_cur;
//Rprintf("NEW:vector[0]=%LF\n",vector[0]);
    cqlsVector_from_vector_to_envR(self->list->vector, vector,envR); 
    cqlsVector_cache_increased_by_weighted_updated_vector_from_envR(cumVector,(DOUBLE)1.0,envR);    
  }
  for(pt=self->oldList->list;pt!=NULL;pt=pt->pt_next) {
    vector=(DOUBLE*)pt->pt_cur;
//Rprintf("OLD:vector[0]=%LF\n",vector[0]);
    cqlsVector_from_vector_to_envR(self->oldList->vector, vector,envR); //oldList->vector==list->vector
    cqlsVector_cache_increased_by_weighted_updated_vector_from_envR(cumVector,(DOUBLE)-1.0,envR);    
  }
//cqlsVector_print_cache(cumVector);Rprintf("done!\n");
}


//Rmk: Normally obsolete! Is it better to split into two contrib new and old?
SEXP ebfunc_vector_cache_cumVector_to_R_as_named_list_from_vector(FUNC_VECTOR_CACHE self,CQLS_VECTOR cumVector,CQLS_R envR) {

  if(cumVector->nbBlockVector==0) return R_NilValue;
  ebfunc_vector_cache_cumVector(self,cumVector,envR);
  return cqlsVector_cache_to_R_as_named_list(cumVector); 
}
    


SEXP ebfunc_vector_cache_lists_to_R_as_named_list(FUNC_VECTOR_CACHE self) {
  SEXP resR,nameR;

//printf("deb\n");
  if(self!=(FUNC_VECTOR_CACHE)NULL) {
//printf("in\n");
    PROTECT(resR=allocVector(VECSXP,2));
    PROTECT(nameR=allocVector(STRSXP,2));
    if(self->list!=(CQLS_VECTOR_LIST)NULL) {
//printf("list\n");cqlsVectorList_print(self->list);
      SET_VECTOR_ELT(resR,0,cqlsVectorList_to_R_as_dataframe(self->list));
    } else SET_VECTOR_ELT(resR,0,R_NilValue);
    if(self->oldList!=(CQLS_VECTOR_LIST)NULL) {
//printf("oldList\n");
      SET_VECTOR_ELT(resR,1,cqlsVectorList_to_R_as_dataframe(self->oldList));
    } else SET_VECTOR_ELT(resR,1,R_NilValue);
    SET_STRING_ELT(nameR,0,mkChar("new"));
    SET_STRING_ELT(nameR,1,mkChar("old"));
    setAttrib(resR,R_NamesSymbol,nameR);
    UNPROTECT(2);
    return resR;
  } else {
//printf("out\n");
    return R_NilValue;
  }
}

//FUNCTION
FUNCTION ebfunc_function_new(FUNC_TYPE id) {
  FUNCTION self;
  self=(FUNCTION)Calloc(1,ST_FUNCTION);
  self->id=id;
  //printf("FUNC_TYPE: self->id=%d\n",self->id);
  self->infoList=NULL;
  self->nbInfoList=0;
  //formula
  ebfunc_function_setFormula(self);
  //infos
  ebfunc_function_setInfo(self);
  //vector
  ebfunc_function_setVector(self);
  //cache and cacheGlob
  self->cacheLoc=(FUNC_VECTOR_CACHE)NULL;
  self->cacheGlob=(FUNC_VECTOR_CACHE)NULL;
  return self;
}

void ebfunc_function_free(FUNCTION self) {
  if(self==(FUNCTION)NULL) return;
  //ebfunc_param_free(self->modpar);
  //Free(self->callR);
  cqlsVector_free(self->vectorLoc);cqlsVector_free(self->vectorCompFuncLoc);
  cqlsVector_free(self->vectorGlob);cqlsVector_free(self->vectorCompFuncGlob);
  Free(self);
  self=NULL;
}

void ebfunc_function_show(FUNCTION self) {
  int i;
  //printf("id=%d,nbParam=%d\n",self->id,self->modpar->nbParam);
  Rprintf("infos:(");for(i=0;i<self->nbInfoList;i++) Rprintf("%s%s",INFO_KEY[self->id][self->infoList[i]],( i==self->nbInfoList-1 ? "" : "," ));printf(")\n");
  Rprintf("vectorLoc:(");
  for(i=0;i<self->vectorLoc->nbBlockVector;i++) Rprintf("%s(%d)",self->vectorLoc->keyBlockVector[i],self->vectorLoc->sizeBlockVector[i]);
  Rprintf(")\n");
   Rprintf("vectorGlob:(");
  for(i=0;i<self->vectorGlob->nbBlockVector;i++) Rprintf("%s(%d)",self->vectorGlob->keyBlockVector[i],self->vectorGlob->sizeBlockVector[i]);
  Rprintf(")\n");
  Rprintf("vectorCompFuncLoc:(");
  for(i=0;i<self->vectorCompFuncLoc->nbBlockVector;i++) Rprintf("%s(%d)",self->vectorCompFuncLoc->keyBlockVector[i],self->vectorCompFuncLoc->sizeBlockVector[i]);
  Rprintf(")\n");
   Rprintf("vectorCompFuncGlob:(");
  for(i=0;i<self->vectorCompFuncGlob->nbBlockVector;i++) Rprintf("%s(%d)",self->vectorCompFuncGlob->keyBlockVector[i],self->vectorCompFuncGlob->sizeBlockVector[i]);
  Rprintf(")\n");
  //printf("callR:%s\n",self->callR);
  Rprintf("cmdString:%s\n",self->R->cmdString);
}


void ebfunc_function_setFormula(FUNCTION self) {
  SEXP form;

  PROTECT(form=ebfunc_eval1("term$form"));
  self->R=cqlsRExpr_new();
  cqlsRExpr_setEnv(self->R,EBFuncEnvName);
  cqlsRExpr_parse(self->R,CHAR(STRING_ELT(form,0)));
  UNPROTECT(1);
}


void ebfunc_function_setInfo(FUNCTION self) {
  int i,j,id;
  char *key;
  SEXP infos;

  PROTECT(infos=ebfunc_eval1("term$infos"));
  id=self->id;
  //BE CAREFUL: id have to be set!!!!
  self->nbInfoList=length(infos);
  self->infoList=(int*)Calloc(self->nbInfoList,int);
  for(i=0;i<self->nbInfoList;i++) {
    key=CHAR(STRING_ELT(infos,i));
    //printf("key[%d]=%s\n",i,key);
    for(j=0;j<INFO_LAST[id];j++) {
      if(strcmp(key,INFO_KEY[id][j])==0)  {
#ifdef debugModeR
	      Rprintf("Info[%d]=%s\n",i,key);
#endif
	      self->infoList[i]=j;
	      break;
      }
    }
    //otherwise, this is a named mark!!!
    
  }
  UNPROTECT(1);
}

void ebfunc_function_addInfo(FUNCTION self) {
  int i,j,id,nb;
  char *key;
  SEXP infos;

  PROTECT(infos=ebfunc_eval1("term$infos"));
  nb=length(infos);
  id=self->id;
  //BE CAREFUL: id have to be set!!!!
  self->infoList=(int*)Realloc(self->infoList,self->nbInfoList+nb,int);
  for(i=0;i<nb;i++) {
    key=CHAR(STRING_ELT(infos,i));
    //printf("key[%d]=%s\n",i,key);
    for(j=0;j<INFO_LAST[id];j++) {
      if(strcmp(key,INFO_KEY[id][j])==0)  {
#ifdef debugModeR
	      Rprintf("Info[%d]=%s\n",self->nbInfoList + i,key);
#endif
	      self->infoList[self->nbInfoList + i]=j;
	      break;
      }
    }  
    //otherwise, this is a named mark!!    
  }
  self->nbInfoList += nb;//printf("NbInfo=%d\n",self->nbInfoList);
  UNPROTECT(1);
}

/* Where to attach the atomic infos! it seems that the more logical is to attach them to structure like graph!
void ebfunc_function_setInfoAtom(FUNCTION self) {
  int i,j,id;
  char *key;
  SEXP infos;

  infos=ebfunc_eval1("term$atomInfos");
  id=self->id;
  //BE CAREFUL: id have to be set!!!!
  self->nbInfoAtomList=length(infos);
  self->atomInfoList=(int*)Calloc(self->nbAtomInfoList,int);
  for(i=0;i<self->nbAtomInfoList;i++) {
    key=CHAR(STRING_ELT(infos,i));
    //printf("key[%d]=%s\n",i,key);
    for(j=0;j<INFO_LAST[id];j++) {
      if(strcmp(key,INFO_KEY[id][j])==0)  {
//#ifdef PRINT_FUNCTION_INIT
	      Rprintf("Info[%d]=%s\n",i,key);
//#endif
	      self->atomInfoList[i]=j;
	      break;
      }
    }
    //otherwise, this is a named mark!!!
    
  }
} */

void ebfunc_function_addVector(FUNCTION self) {
  int i;
  SEXP size,comps,keys;

  PROTECT(comps=ebfunc_eval1("term$caracLoc"));
  PROTECT(size=ebfunc_eval1("term$caracLoc.size"));
  PROTECT(keys=ebfunc_eval1("names(term$caracLoc)"));
  for(i=0;i<length(comps);i++)
    cqlsVector_add_block(self->vectorLoc, INTEGER(size)[i], CHAR(STRING_ELT(keys,i)), CHAR(STRING_ELT(comps,i)));

  comps=ebfunc_eval1("term$compFuncLoc");
  size=ebfunc_eval1("term$compFuncLoc.size");
  keys=ebfunc_eval1("names(term$compFuncLoc)");
  for(i=0;i<length(comps);i++)
    cqlsVector_add_block(self->vectorCompFuncLoc, INTEGER(size)[i], CHAR(STRING_ELT(keys,i)), CHAR(STRING_ELT(comps,i)));

  comps=ebfunc_eval1("term$caracGlob");
  size=ebfunc_eval1("term$caracGlob.size");
  keys=ebfunc_eval1("names(term$caracGlob)");
  for(i=0;i<length(comps);i++)
    cqlsVector_add_block(self->vectorGlob, INTEGER(size)[i], CHAR(STRING_ELT(keys,i)), CHAR(STRING_ELT(comps,i)));

  comps=ebfunc_eval1("term$compFuncGlob");
  size=ebfunc_eval1("term$compFuncGlob.size");
  keys=ebfunc_eval1("names(term$compFuncGlob)");
  for(i=0;i<length(comps);i++)
    cqlsVector_add_block(self->vectorCompFuncGlob, INTEGER(size)[i], CHAR(STRING_ELT(keys,i)), CHAR(STRING_ELT(comps,i)));
  UNPROTECT(3);
}

void ebfunc_function_setVector(FUNCTION self) {
  int i;

  self->vectorLoc=cqlsVector_new();
  self->vectorCompFuncLoc=cqlsVector_new();
  self->vectorGlob=cqlsVector_new();
  self->vectorCompFuncGlob=cqlsVector_new();
  ebfunc_function_addVector(self);
}

/*************************************/
/*  Common part for term FUNC_TERM_  */
/*************************************/

/*
void ebfunc_term_new_(FUNC_TERM_ self, FUNCTION function) {
  self->function=function;

  //self->nbFunctionList=0;
  //self->functionList=NULL;
  //self->nbFunctionExpoList=0;
  //self->functionExpoList=NULL;
}

void ebfunc_term_free_(FUNC_TERM_ self) {
  int i;

  //free element of functionList if last used (made in ebfunc_function_free_)
  for(i=0;i<self->nbFunctionList;i++) ebfunc_function_free(self->functionList[i]);
  Free(self->functionList);
  for(i=0;i<self->nbFunctionExpoList;i++) ebfunc_function_free(self->functionExpoList[i]);
  Free(self->functionExpoList);
}

void ebfunc_term_add_function(FUNC_TERM_ self, FUNCTION function) {

  int *nbFunctionList;
  FUNCTION **functionList;

  if(FUNC_TYPE_EXPO(function->id)) {
    nbFunctionList = &(self->nbFunctionExpoList);
    functionList=&(self->functionExpoList);
  } else {
    nbFunctionList = &(self->nbFunctionList);
    functionList=&(self->functionList);
  }
  Rprintf("nbEner=%d\n",*nbFunctionList);
  (*nbFunctionList)++;
  *functionList=(FUNCTION*)Realloc(*functionList,(*nbFunctionList),FUNCTION);
  *functionList[(*nbFunctionList)-1]=function;
//printf("self=%d,functionList=%d,idTerm=%d,functionList[%d]=%d\n",self,*functionList,FUNC_TERM_ID(function->id),(*nbFunctionList)-1,functionList[(*nbFunctionList)-1]);
  //printf("term_add:fucnction=%d\n",self->function);
  //update self->info
  ebfunc_info_add(self->info, function->infoList, function->nbInfoList,0);
}*/



/****************/
/*  Singleton   */
/****************/
//Func expo side!
DOUBLE ebfunc_get_single() {
  SEXP res;

  res=ebfunc_eval1("Single");
  return cqlsSEXP_DBL(res,0);
}

//Func expo side!
//void ebfuncExpo_functionSingle_componentUpdate(DOUBLE *component) {
//  component[0] += (DOUBLE)1.0;
//}

/****************/
/*    Del1      */
/****************/
/////////////////////////// Component termDel1
COMPONENT_TERM_DEL1 ebcomponent_termDel1_new() {
  COMPONENT_TERM_DEL1 self;

  self=(COMPONENT_TERM_DEL1)Calloc(1,ST_COMPONENT_TERM_DEL1);
  self->info=ebfunc_info_new(INFO_DEL1_LAST);
  return self;
}

void ebcomponent_termDel1_free(COMPONENT_TERM_DEL1 self) {
 if (self!=(COMPONENT_TERM_DEL1)NULL) {
    ebfunc_info_free(self->info);
    Free(self);
  }
}

void ebcomponent_termDel1_set(COMPONENT_TERM_DEL1 self,PT_DV point1) {
  self->point1=point1;
}

//RMK: NOW NO USE OF SELF->INFOS!!!!
void ebcomponent_termDel1_updateInfo(COMPONENT_TERM_DEL1 self,PT_POLY poly) {
  int i,infoId,nbRes,id[1];
  DOUBLE res;

  for(i=0;i<self->info->nbInfoList;i++) {
    infoId=self->info->infoList[i];
    switch(infoId) {
      case INFO_DEL1_ID: {
        //to check! Never used
        id[0]=self->point1-poly->vg->tab_dv;
        cqlsR_INT2SEXP(EBFUNC_R,INFO_KEY_DEL1[infoId],id,1);
	      break;
      }
      case INFO_DEL1_X: {
        //to check! Never used
        cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL1[infoId],(*(self->point1))->point,2);
	      break;
      }
      case INFO_DEL1_V: {
	      DC_curObj(poly->vg->dc,*(self->point1));
        //printf("DC_INT(poly->vg->dc,0)[0]=%d \n",DC_INT(poly->vg->dc,0)[0]);
	      cqlsR_setAllVars_List_new(EBFUNC_R,INFO_KEY_DEL1[infoId],poly->vg->dc);
        //cqlsR_eval1(EBFUNC_R,"print(v)"); //==> this works! 
        //cqlsSEXP_setAllVars_MDF(poly->vg->marks,k,pt_vor->dc); //=> not this one
	      break;
         /*PROTECT(resR=allocVector(VECSXP,2));//resR<-list() with 2 elements 
	      DC_curObj(poly->vg->dc,*point1);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->marks->nbData));//tmpR <- list()
	      DU_cqlsSEXP_setAllVars_List(tmpR,poly->vg->marks);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR*/
      }
      case INFO_DEL1_A: {
	      res=ebdv_area(self->point1,poly);
	      //printf("area=%LF\n",res);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL1[infoId],&res,1);
	      break;
      }
    }
  }
}

/////////////////////////// Func termDel1
FUNC_TERM_DEL1 ebfunc_termDel1_new(FUNCTION function) {
  FUNC_TERM_DEL1 self;
  self=(FUNC_TERM_DEL1)Calloc(1,ST_FUNC_TERM_DEL1);
  self->component=ebcomponent_termDel1_new();
  self->info=self->component->info;
  self->function=function;
  ebfunc_info_add(self->info, function->infoList, function->nbInfoList,0);
  return self;
}

void ebfunc_termDel1_free(FUNC_TERM_DEL1 self) {
  if (self!=(FUNC_TERM_DEL1)NULL) {
    //TODO:ebfunc_free_function(self->function);
    ebcomponent_termDel1_free(self->component);
    Free(self);
  }
}

//////////////////////////////////////////////////////Func non expo side!!!
DOUBLE ebfunc_termDel1_function_component(FUNC_TERM_DEL1 self,PT_POLY poly) {
  //DOUBLE enloc=0.0;
  int i;
  
  ebcomponent_termDel1_updateInfo(self->component,poly);
  //only non expo !
  //for(i=0;i<self->nbFunctionList;i++) {
    cqlsVector_eval(self->function->vectorCur,EBFUNC_R);//printf("ici2\n");
    cqlsRExpr_eval(self->function->R);   //printf("ici3\n");
    return cqlsSEXP_DBL(self->function->R->ans,0);
  //}
  //return enloc;
}

DOUBLE ebfunc_termDel1_function_componentList(FUNC_TERM_DEL1 self, PT_LIST pt_list,PT_POLY poly) {
  ////PARTIE COMMUNE   
  PT_LIST pt,dvList;
  PT_DV dv;
  
 //initialisation 
  DOUBLE enloc=0.0;
  //détermination des
  dvList=NULL;
  ebvor_dvList_in_vvList(poly->vg,pt_list,&dvList,1);
  //parcourrir les points
  while(dvList!=NULL) {
    dv=(PT_DV) eblist_recup_tete(&dvList);    
    ebcomponent_termDel1_set(self->component,dv);
    //ebvor_control_dv(poly->vg,point1,"function:");
    enloc += ebfunc_termDel1_function_component(self,poly);
  } //dvList already cleaned
  ////FIN PARTIE COMMUNE
  return enloc;
}

////SEULE INTERFACE AU NIVEAU ENERGIE!!!
DOUBLE ebfunc_termDel1_localEnergy(FUNC_TERM_DEL1 self, PT_POLY poly) {
  DOUBLE enloc=0.0;

  self->function->vectorCur=self->function->vectorLoc;
  ebpoly_apply(poly);
  enloc += ebfunc_termDel1_function_componentList(self,poly->list_vvs_nouveaux,poly); 
  ebpoly_cancel(poly); 
  enloc -= ebfunc_termDel1_function_componentList(self,poly->list_vvs_supprimes,poly); 
  enloc*=(DOUBLE)poly->mode;
  
  return enloc;
}

DOUBLE ebfunc_termDel1_globalEnergy(FUNC_TERM_DEL1 self, PT_POLY poly) {
  PT_DV dv; 
  int i;
  //initialisation 
  DOUBLE encomp,englob=0.0; 

  self->function->vectorCur=self->function->vectorGlob;
  for(i=0;i<poly->vg->nb_dv;i++) {
    dv=(poly->vg->tab_dv)+i;
    if(*dv==NULL) continue;
    if((*dv)->num_s==-3) continue;
    ebcomponent_termDel1_set(self->component,dv);
    //ebvor_control_dv(poly->vg,point1,"function:");
    //Rprintf("del1 term in englob=%LF\n",ebfunc_termDel1_function_component(self,poly));
    englob += ebfunc_termDel1_function_component(self,poly);
  }

  return englob;
}
//////////////////////////////////////////////////////Func component list side!!!
void ebfunc_termDel1_update_component(FUNC_TERM_DEL1 self,CQLS_VECTOR_LIST vectorCache,PT_POLY poly) {

  ebcomponent_termDel1_updateInfo(self->component,poly);
  cqlsVectorList_insert(vectorCache,cqlsVector_from_envR_to_new_vector_after_update(self->function->vectorCur,EBFUNC_R));
}

void ebfunc_termDel1_update_componentList(FUNC_TERM_DEL1 self,CQLS_VECTOR_LIST vectorCache,PT_LIST pt_list, PT_POLY poly) {
  ////PARTIE COMMUNE
  PT_LIST pt,dvList;
  int i,i1,i2;
  PT_DV dv;
  
 //initialisation 
  DOUBLE enloc=0.0;
  //détermination des
  dvList=NULL;
  ebvor_dvList_in_vvList(poly->vg,pt_list,&dvList,1);
  //parcourrir les aretes
  while(dvList!=NULL) {
    dv=(PT_DV)eblist_recup_tete(&dvList);
    ebcomponent_termDel1_set(self->component,dv);
    ebfunc_termDel1_update_component(self,vectorCache,poly);
      ////FIN PARTIE MODIFIABLE
  } //no need to clean del_list (since already done)!
  ////FIN PARTIE COMMUNE
}

//SEULE INTERFACE UTILSEE PAR FUNC COMPONENT LIST!!!
void ebfunc_termDel1_componentLocalUpdate(FUNC_TERM_DEL1 self,PT_POLY poly) {
  CQLS_VECTOR_LIST vectorCache;

  self->function->vectorCur=self->function->vectorLoc;
  ebpoly_apply(poly);
  //printf("poly->mode=%d\n",poly->mode);
  if (poly->mode==1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;
  ebfunc_termDel1_update_componentList(self,vectorCache,poly->list_vvs_nouveaux,poly); 


  ebpoly_cancel(poly);
  if (poly->mode==-1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;

  ebfunc_termDel1_update_componentList(self,vectorCache,poly->list_vvs_supprimes,poly); 
}

void ebfunc_termDel1_componentGlobalUpdate(FUNC_TERM_DEL1 self,PT_POLY poly) {
   PT_DV dv; 
  int i;
  //initialisation 
  DOUBLE encomp,englob=0.0; 
  CQLS_VECTOR_LIST vectorCache;
    
  self->function->vectorCur=self->function->vectorGlob;
  vectorCache=self->function->cacheGlob->list;
  for(i=0;i<poly->vg->nb_dv;i++) {
    dv=(poly->vg->tab_dv)+i;
    if(*dv==NULL) continue;
    if((*dv)->num_s==-3) continue;
    ebcomponent_termDel1_set(self->component,dv);
    //ebvor_control_dv(poly->vg,point1,"function:");
    //Rprintf("del1 term in englob=%LF\n",ebfunc_termDel1_function_component(self,poly));
    ebfunc_termDel1_update_component(self,vectorCache,poly);
  }
  //cqlsVectorList_print(vectorCache);
}

/****************/
/*    Del2      */
/****************/
/////////////////////////// Component termDel2
COMPONENT_TERM_DEL2 ebcomponent_termDel2_new() {
  COMPONENT_TERM_DEL2 self;

  self=(COMPONENT_TERM_DEL2)Calloc(1,ST_COMPONENT_TERM_DEL2);
  self->info=ebfunc_info_new(INFO_DEL2_LAST);
  return self;
}

void ebcomponent_termDel2_free(COMPONENT_TERM_DEL2 self) {
 if (self!=(COMPONENT_TERM_DEL2)NULL) {
    ebfunc_info_free(self->info);
    Free(self);
  }
}


void ebcomponent_termDel2_set(COMPONENT_TERM_DEL2 self,PT_DV point1,PT_DV point2,PT_VV sommet,PT_VV sommetv) {
  self->point1=point1;
  self->point2=point2;
  //dual edge (used for length ortho edge)
  self->sommet=sommet;
  self->sommetv=sommetv;
}

void ebcomponent_termDel2_setFromSommet(COMPONENT_TERM_DEL2 self,PT_VV sommet,int side) {
int i1,i2;  
  self->sommet=sommet;
  ebutil_indices_arete(side,&i1,&i2);
  self->point1=(*sommet)->ndvs[i1];
  self->point2=(*sommet)->ndvs[i2];
  //dual edge (used for length ortho edge)
  self->sommetv=(*sommet)->nvvs[side];
}

int ebcomponent_termDel2_unproper(COMPONENT_TERM_DEL2 self,PT_POLY poly) {
return (self->point1-poly->vg->tab_dv<0 | self->point2-poly->vg->tab_dv<0);
}

//RMK: NOW NO USE OF SELF->INFOS!!!!
void ebcomponent_termDel2_updateInfo(COMPONENT_TERM_DEL2 self,PT_POLY poly) {
  int i,ii,infoId,nbRes;
  DOUBLE res,*res2;
  int id[2];
  SEXP resR,tmpR;
  PT_DV point1=self->point1,point2=self->point2;

  //printf("updateInfo Del2:nbInfo=%d\n",self->info->nbInfoList);
  //for(i=0;i<self->info->nbInfoList;i++) Rprintf("InfoId=%d\n",self->info->infoList[i]);

  for(i=0;i<self->info->nbInfoList;i++) {
    infoId=self->info->infoList[i];
    //printf("Update Del2 -> InfoId=%d\n",infoId);
    switch(infoId) {
      case INFO_DEL2_ID: {
        //to check! Never used
        id[0]=(int)(point1-poly->vg->tab_dv);id[1]=(int)(point2-poly->vg->tab_dv);
#ifdef debugModeR
        Rprintf("point1=%d,point2=%d\n",id[0],id[1]);
#endif
        cqlsR_INT2SEXP(EBFUNC_R,INFO_KEY_DEL2[infoId],id,2);
	      break;
      }
      case INFO_DEL2_X: {//Vector of Coordinates
        PROTECT(resR=allocVector(VECSXP,2));//resR<-list() with 2 elements 
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
        for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point1)->point[ii]);
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
	      for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point2)->point[ii]);
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_DEL2[infoId]),resR,EBFUNC_R->env);// assign("x",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      }
      case INFO_DEL2_V: { //marks points
	      PROTECT(resR=allocVector(VECSXP,2));//resR<-list() with 2 elements 
	      DC_curObj(poly->vg->dc,*point1);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->marks->nbData));//tmpR <- list()
	      DU_cqlsSEXP_setAllVars_List(tmpR,poly->vg->marks);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      DC_curObj(poly->vg->dc,*point2);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->marks->nbData));//tmpR <- list()
	      DU_cqlsSEXP_setAllVars_List(tmpR,poly->vg->marks);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_DEL2[infoId]),resR,EBFUNC_R->env);// assign("v",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      }
      case INFO_DEL2_A: {//area point1-> do not use now!!!
	      res2=(DOUBLE*)Calloc(2,DOUBLE);
	      res2[0]=ebdv_area(point1,poly);
	      //printf("area[0]=%LF\n",res2[0]);
	      res2[1]=ebdv_area(point2,poly);
	      //printf("area[1]=%LF\n",res2[1]);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL2[infoId],res2,2);
	      Free(res2);
	      break;
      }
      case INFO_DEL2_L2: {
	      //length edge
	      res=ebcalcul_distance((*point1)->point,(*point2)->point);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL2[infoId],&res,1);
	      //printf("l2=%LF\n",res);
	      break;
      }
      case INFO_DEL2_L: {
	      //length edge
	      res=sqrt(ebcalcul_distance((*point1)->point,(*point2)->point));
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL2[infoId],&res,1);
	      //printf("l=%LF\n",res);
	      break;
      }
      case INFO_DEL2_OL2: {//Square of orthogonal edge length
        res=ebcalcul_distance((*(self->sommet))->point,(*(self->sommetv))->point);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL2[infoId],&res,1);
	      break;
      }
      case INFO_DEL2_OL: {//Orthogonal edge length
        res=sqrt(ebcalcul_distance((*(self->sommet))->point,(*(self->sommetv))->point));
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL2[infoId],&res,1);
	      break;
      }
      case INFO_DEL2_DA: {//Difference of two areas
        res=ebdv_area(point1,poly);
        //Rprintf("A(point1=%LF)-A(point2=%LF)",res,ebdv_area(point2,poly));
	      res=fabs(res-ebdv_area(point2,poly));
        //printf("da=%LF\n",res);
        cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL2[infoId],&res,1);
      }
    }
    //printf("OUT\n");
  }
}
/////////////////////////// Func termDel2
FUNC_TERM_DEL2 ebfunc_termDel2_new(FUNCTION function) {
  FUNC_TERM_DEL2 self;
  self=(FUNC_TERM_DEL2)Calloc(1,ST_FUNC_TERM_DEL2);
  self->component=ebcomponent_termDel2_new();
  self->info=self->component->info;
  self->function=function;
  self->order=INTEGER(ebfunc_eval1("as.integer(if(is.null(term$args$order)) 0 else term$args$order)"))[0];//O even if order is actually 1 but with different method! 
#ifdef debugModeR
  Rprintf("Del2 order=%d\n",self->order);
#endif
  ebfunc_info_add(self->info, function->infoList, function->nbInfoList,0);
  return self;
}

void ebfunc_termDel2_free(FUNC_TERM_DEL2 self) {
  if (self!=(FUNC_TERM_DEL2)NULL) {
    ebcomponent_termDel2_free(self->component);
    Free(self);
  }
}

//////////////////////////////////////////////////////Func non expo side!!!
DOUBLE ebfunc_termDel2_function_component(FUNC_TERM_DEL2 self,PT_POLY poly) {
  DOUBLE enloc=0.0;
  int i;
  
  //update the info from poly!
  ebcomponent_termDel2_updateInfo(self->component,poly);//printf("ici\n");
    cqlsVector_eval(self->function->vectorCur,EBFUNC_R);//printf("ici2\n");
    cqlsRExpr_eval(self->function->R);   //printf("ici3\n");
    enloc = cqlsSEXP_DBL(self->function->R->ans,0);//printf("ici4\n");
  return enloc;
}

DOUBLE ebfunc_termDel2_function_componentList(FUNC_TERM_DEL2 self, PT_LIST pt_list,PT_POLY poly) {
  ////PARTIE COMMUNE  ENERGIE PAIRE LIST
  PT_LIST pt,nvvsList=NULL;
  PT_VV vv,nvv; 
  int i;
  //initialisation 
  DOUBLE enloc=0.0; 

  //init nvvsList
  if(self->order==0) {//in fact order 1 too
    //remettre les labels à 0!
    //ebpoly_init_label(poly); //TODO: normally, should be done at the end and not at the begininng!
    nvvsList=pt_list; //ATTENTION: this is not a copy so no deletion
  } else {
    ebvor_nvvs_for_vvList_until_order(poly->vg,pt_list,self->order,&nvvsList);
  }

  //run over nvvsList elements
  for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {
    vv=(PT_VV)pt->pt_cur;
    for(i=0;i<3;i++) {
      nvv=(*vv)->nvvs[i];
      if(nvv==NULL) continue;
      if((*nvv)->ind != 0) continue;
      ////PARTIE MODIFIABLE SELON FCT INTERACTION
      //points de l'arête dans la direction i!!!
      ebcomponent_termDel2_setFromSommet(self->component,vv,i);
//#ifdef TEST_SUP
      //printf("paire[(%LF,%LF)(%LF,%LF)]=%LF\n",(*point1)->point[0],(*point1)->point[1],(*point2)->point[0],(*point2)->point[1],ebsim_straussDelEdge_eval(self,(*point1)->point,(*point2)->point));
//#endif
      if(ebcomponent_termDel2_unproper(self->component,poly)) continue;
      enloc += ebfunc_termDel2_function_component(self,poly);
      ////FIN PARTIE MODIFIABLE
    }
    (*vv)->ind=1;
  }
  
  //clean label to be polite!
  if(self->order==0) //do not destroy nvvsList which is pt_list here!
    for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {vv=(PT_VV)pt->pt_cur;(*vv)->ind=0;}
  else 
    while(nvvsList!=NULL) {vv=(PT_VV)eblist_recup_tete(&nvvsList);(*vv)->ind=0;}
     
  return enloc;
  ////FIN PARTIE COMMUNE
}

////SEULE INTERFACE AU NIVEAU ENERGIE!!!
DOUBLE ebfunc_termDel2_localEnergy(FUNC_TERM_DEL2 self, PT_POLY poly) {
  DOUBLE enloc=0.0;

  self->function->vectorCur=self->function->vectorLoc;
  ebpoly_apply(poly);
  enloc += ebfunc_termDel2_function_componentList(self,poly->list_vvs_nouveaux,poly);
  ebpoly_cancel(poly);
  enloc -= ebfunc_termDel2_function_componentList(self,poly->list_vvs_supprimes,poly);
  
  enloc*=(DOUBLE)poly->mode;
  
  return enloc;
}

DOUBLE ebfunc_termDel2_globalEnergy(FUNC_TERM_DEL2 self, PT_POLY poly) {
  DOUBLE enloc=0.0;
  int i;
  PT_VV sommet;
  PT_LIST list_vvs=NULL;
 
  for(i=0;i<poly->vg->nb_vv;i++) {
    sommet = (PT_VV)(poly->vg->tab_vv+i);
    if(ebvv_isDispo(sommet)) continue;
//printf("sommet[%d]=%d\n",i,sommet);
    eblist_ins_tete(&(list_vvs),sommet);
  }
//printf("ici2!!!\n");
  //ebvor_vv_init_label(poly->vg);
//printf("ici3!!!\n");
  self->function->vectorCur=self->function->vectorGlob;//printf("ici3!!!\n");
//printf("iciii222!!!\n");
  enloc=ebfunc_termDel2_function_componentList(self,list_vvs,poly);

  return enloc;
}

//////////////////////////////////////////////////////Func component list side!!!
void ebfunc_termDel2_update_component(FUNC_TERM_DEL2 self,CQLS_VECTOR_LIST vectorCache, PT_POLY poly) {
  
  ebcomponent_termDel2_updateInfo(self->component,poly);
  cqlsVectorList_insert(vectorCache,cqlsVector_from_envR_to_new_vector_after_update(self->function->vectorCur,EBFUNC_R));
}

void ebfunc_termDel2_update_componentList(FUNC_TERM_DEL2 self,CQLS_VECTOR_LIST vectorCache,PT_LIST pt_list, PT_POLY poly) {
  ////PARTIE COMMUNE
  PT_LIST pt,nvvsList;
  PT_VV vv,nvv;
  int i;


/*OLD
  //printf("list is NULL=%d\n",(pt_list==NULL ? 1 : 0));
  //mettre les labels à 0!
  //ebpoly_init_label(poly); => PUT IN componentUpdate TO BE USEABLE FOT DEL2GLOB!
  //parcourrir les arêtes
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
  sommet=(PT_VV)pt->pt_cur;
//printf("debug: sommet-poly->vg->tab_vv=%d\n",sommet-poly->vg->tab_vv);
  //if(sommet-poly->vg->tab_vv<0) continue;
#ifdef DEBUG_PSEUDO_COMPONENT_EDGE
  Rprintf("sommet=%d\n",*sommet);
#endif
  for(i=0;i<3;i++) {
    voisin=(*sommet)->nvvs[i];
#ifdef DEBUG_PSEUDO_COMPONENT_EDGE
    Rprintf("voisin=%d\n",*voisin);
#endif
    if(*voisin==NULL) continue;
#ifdef DEBUG_PSEUDO_COMPONENT_EDGE
      Rprintf("voisin->label=%d\n",(*voisin)->label);
#endif
    if((*voisin)->label != 0) continue;
      ////PARTIE MODIFIABLE SELON FCT INTERACTION
      //points de l'arête dans la direction i!!!
      ebcomponent_termDel2_setFromSommet(self->component,sommet,i);
      if(ebcomponent_termDel2_unproper(self->component,poly)) continue;
      ebfunc_termDel2_update_component(self,vectorCache,poly);
      ////FIN PARTIE MODIFIABLE
  }
  (*sommet)->label=1;
*/

  //init nvvsList
  if(self->order==0) {//in fact order 1 too
    nvvsList=pt_list; //ATTENTION: this is not a copy so no deletion
  } else {
    ebvor_nvvs_for_vvList_until_order(poly->vg,pt_list,self->order,&nvvsList);
  }

  //run over nvvsList elements
  for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {
    vv=(PT_VV)pt->pt_cur;
    for(i=0;i<3;i++) {
      nvv=(*vv)->nvvs[i];
      if(nvv==NULL) continue;
      if((*nvv)->ind != 0) continue;
      ////PARTIE MODIFIABLE SELON FCT INTERACTION
      //points de l'arête dans la direction i!!!
      ebcomponent_termDel2_setFromSommet(self->component,vv,i);
//#ifdef TEST_SUP
      //printf("paire[(%LF,%LF)(%LF,%LF)]=%LF\n",(*point1)->point[0],(*point1)->point[1],(*point2)->point[0],(*point2)->point[1],ebsim_straussDelEdge_eval(self,(*point1)->point,(*point2)->point));
//#endif
      if(ebcomponent_termDel2_unproper(self->component,poly)) continue;
      ebfunc_termDel2_update_component(self,vectorCache,poly);
      ////FIN PARTIE MODIFIABLE
    }
    (*vv)->ind=1;
  }
  //clean label to be polite!
  if(self->order==0) 
    for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {vv=(PT_VV)pt->pt_cur;(*vv)->ind=0;}
  else 
    while(nvvsList!=NULL) {vv=(PT_VV)eblist_recup_tete(&nvvsList);(*vv)->ind=0;}
     
  ////FIN PARTIE COMMUNE
}

//SEULE INTERFACE UTILSEE PAR FUNC COMPONENT!!!
void ebfunc_termDel2_componentLocalUpdate(FUNC_TERM_DEL2 self,PT_POLY poly) {
CQLS_VECTOR_LIST vectorCache;

  self->function->vectorCur=self->function->vectorLoc;  
  ebpoly_apply(poly);
  //printf("poly->mode=%d\n",poly->mode);
  if (poly->mode==1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;
  ebpoly_init_label(poly); //mettre les labels à 0!
  ebfunc_termDel2_update_componentList(self,vectorCache,poly->list_vvs_nouveaux,poly); 


  ebpoly_cancel(poly);
  if (poly->mode==-1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;
  ebpoly_init_label(poly);//mettre les labels à 0!
  ebfunc_termDel2_update_componentList(self,vectorCache,poly->list_vvs_supprimes,poly); 
}

/***************/
/* Del2 Global */
/***************/
//same thing as ebfunc_termDel2_componentUpdate execpt that the list is bigger!
//and there is no negative contribution!

void ebfunc_termDel2_componentGlobalUpdate(FUNC_TERM_DEL2 self,PT_POLY poly) {
  int i;
  PT_VV sommet;
  PT_LIST list_vvs=NULL;
  CQLS_VECTOR_LIST vectorCache;
  
//printf("Del2GU!!!!\n");
  for(i=0;i<poly->vg->nb_vv;i++) {
    sommet = (PT_VV)(poly->vg->tab_vv+i);
    if(ebvv_isDispo(sommet)) continue;
//printf("sommet[%d]=%d\n",i,sommet);
    eblist_ins_tete(&(list_vvs),sommet);
  }
//printf("Del2GU2!!!\n");
  ebvor_all_vvs_init_label(poly->vg);
//printf("Del2U3!!!\n");
  self->function->vectorCur=self->function->vectorGlob;//printf("ici3!!!\n");
  vectorCache=self->function->cacheGlob->list;//printf("ici3!!!\n");
//printf("Del2GU4!!!->%d\n",vectorCache);
  ebfunc_termDel2_update_componentList(self,vectorCache,list_vvs,poly);
//printf("globUpdate\n");cqlsVectorList_print(vectorCache); 
}



/****************/
/*    Del3      */
/****************/
/////////////////////////// Component termDel3
COMPONENT_TERM_DEL3 ebcomponent_termDel3_new() {
  COMPONENT_TERM_DEL3 self;

  self=(COMPONENT_TERM_DEL3)Calloc(1,ST_COMPONENT_TERM_DEL3);
  self->info=ebfunc_info_new(INFO_DEL3_LAST);
  return self;
}

void ebcomponent_termDel3_free(COMPONENT_TERM_DEL3 self) {
 if (self!=(COMPONENT_TERM_DEL3)NULL) {
    ebfunc_info_free(self->info);
    Free(self);
  }
}

 
void ebcomponent_termDel3_set(COMPONENT_TERM_DEL3 self,PT_VV sommet) {
  self->sommet=sommet;
  self->point1=(*sommet)->ndvs[0];
  self->point2=(*sommet)->ndvs[1];
  self->point3=(*sommet)->ndvs[2];
}

//RMK: NOW NO USE OF SELF->INFOS!!!!
void ebcomponent_termDel3_updateInfo(COMPONENT_TERM_DEL3 self,PT_POLY poly) {
  int i,ii,infoId,nbRes,id[3];
  DOUBLE res,*res2;
  SEXP resR,tmpR;
  PT_DV point1=self->point1,point2=self->point2,point3=self->point3;

  for(i=0;i<self->info->nbInfoList;i++) {
    infoId=self->info->infoList[i];
    switch(infoId) {
      case INFO_DEL3_ID: {
        //to check! Never used
        id[0]=(int)(point1-poly->vg->tab_dv);id[1]=(int)(point2-poly->vg->tab_dv);id[2]=(int)(point3-poly->vg->tab_dv);
#ifdef debugModeR
        Rprintf("point1=%d,point2=%d,point3=%d\n",id[0],id[1],id[2]);
#endif
        cqlsR_INT2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],id,3);
	      break;
      }
      case INFO_DEL3_X: {//Vector of Coordinates
        PROTECT(resR=allocVector(VECSXP,3));//resR<-list() with 2 elements 
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
        for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point1)->point[ii]);
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
	      for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point2)->point[ii]);
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
        PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
	      for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point3)->point[ii]);
	      SET_VECTOR_ELT(resR,2,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_DEL3[infoId]),resR,EBFUNC_R->env);// assign("v",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      }
      case INFO_DEL3_V: {//Vector of marks
	      PROTECT(resR=allocVector(VECSXP,3));//resR<-list() with 2 elements
	      DC_curObj(poly->vg->dc,*point1);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->dc->n));//tmpR <- list()
	      cqlsSEXP_setAllVars_List(tmpR,poly->vg->dc);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      DC_curObj(poly->vg->dc,*point2);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->dc->n));//tmpR <- list()
	      cqlsSEXP_setAllVars_List(tmpR,poly->vg->dc);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      DC_curObj(poly->vg->dc,*point3);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->dc->n));//tmpR <- list()
	      cqlsSEXP_setAllVars_List(tmpR,poly->vg->dc);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,2,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_DEL2[infoId]),resR,EBFUNC_R->env);// assign("v",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      }
      case INFO_DEL3_A: {//Vector of 3 areas
	      res2=(DOUBLE*)Calloc(3,DOUBLE);
	      res2[0]=ebdv_area(point1,poly);
	      res2[1]=ebdv_area(point2,poly);
	      res2[2]=ebdv_area(point3,poly);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],res2,3);
	      free(res2);
	      break;
      }
      case INFO_DEL3_TA: {//Triangle area
        res=ebvv_area(self->sommet);
        //Rprintf("triangle area=%LF\n",res);
        cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],&res,1);
	      break;
      }
      case INFO_DEL3_TP: {//Triangle perimeter
        res=ebvv_perimeter(self->sommet);
        //Rprintf("triangle area=%LF\n",res);
        cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],&res,1);
	      break;
      }
      case INFO_DEL3_C: {//Coordinates of the center
        //to check! Never used
        cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],(*(self->sommet))->point,2);
	      break;
      }
      case INFO_DEL3_R2: {//radius^2
	      res=ebcalcul_distance((*point1)->point,(*(self->sommet))->point);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],&res,1);
	      break;
      }
      case INFO_DEL3_R: {//radius
	      res=sqrt(ebcalcul_distance((*point1)->point,(*(self->sommet))->point));
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],&res,1);
	      break;
      }
      case INFO_DEL3_SA: {//Smallest Angle
	      res=ebvv_petitAngle(self->sommet);
//printf("sa=%LF\n",res);
        cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],&res,1);
	      break;
      }
      case INFO_DEL3_GA: {//Greatest Angle
	      res=ebvv_grandAngle(self->sommet);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_DEL3[infoId],&res,1);
	      break;
      }
    }
  }
}
/////////////////////////// Func termDel3
void ebfunc_termDel3_free(FUNC_TERM_DEL3 self) {
  if (self!=(FUNC_TERM_DEL3)NULL) {
    //TODO???ebfunc_term_free_((FUNC_TERM_)self);
    ebcomponent_termDel3_free(self->component);
    Free(self);
  }
}

FUNC_TERM_DEL3 ebfunc_termDel3_new(FUNCTION function) {
  FUNC_TERM_DEL3 self;
  self=(FUNC_TERM_DEL3)Calloc(1,ST_FUNC_TERM_DEL3);
  self->component=ebcomponent_termDel3_new();
  self->info=self->component->info;
  self->function=function;
  ebfunc_info_add(self->info, function->infoList, function->nbInfoList,0);
  return self;
}


//////////////////////////////////////////////////////Func non expo side!!!
DOUBLE ebfunc_termDel3_function_component(FUNC_TERM_DEL3 self,PT_POLY poly) {
  DOUBLE enloc=0.0;
  int i;
  
  ebcomponent_termDel3_updateInfo(self->component,poly);
    cqlsVector_eval(self->function->vectorCur,EBFUNC_R);//printf("ici2\n");
    cqlsRExpr_eval(self->function->R);   //printf("ici3\n");
    enloc = cqlsSEXP_DBL(self->function->R->ans,0);//printf("ici4\n");
  return enloc;
}

DOUBLE ebfunc_termDel3_function_componentList(FUNC_TERM_DEL3 self, PT_LIST pt_list,PT_POLY poly) {
  ////PARTIE COMMUNE  ENERGIE PAIRE LIST
  PT_LIST pt;
  PT_VV sommet,voisin;
  int i,i1,i2;
  
 //initialisation 
  DOUBLE enloc=0.0;
  ebpoly_init_label(poly);
  //parcourrir les ar�es
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
    sommet=(PT_VV)pt->pt_cur;
    ebcomponent_termDel3_set(self->component,sommet);
    enloc += ebfunc_termDel3_function_component(self,poly);
    ////FIN PARTIE MODIFIABLE
  }
  return enloc;
  ////FIN PARTIE COMMUNE
}

////SEULE INTERFACE AU NIVEAU ENERGIE!!!
DOUBLE ebfunc_termDel3_localEnergy(FUNC_TERM_DEL3 self, PT_POLY poly) {
  DOUBLE enloc=0.0;

  self->function->vectorCur=self->function->vectorLoc;
  ebpoly_apply(poly);
  enloc += ebfunc_termDel3_function_componentList(self,poly->list_vvs_nouveaux,poly);
  ebpoly_cancel(poly);
  enloc -= ebfunc_termDel3_function_componentList(self,poly->list_vvs_supprimes,poly);
  
  enloc*=(DOUBLE)poly->mode;
  
  return enloc;
}

DOUBLE ebfunc_termDel3_globalEnergy(FUNC_TERM_DEL3 self, PT_POLY poly) {
  DOUBLE englob=0.0;
  int i;
  PT_VV sommet;
  PT_LIST list_vvs=NULL;
 
  for(i=0;i<poly->vg->nb_vv;i++) {
    sommet = (PT_VV)(poly->vg->tab_vv+i);
    if(ebvv_isDispo(sommet)) continue;
//printf("sommet[%d]=%d\n",i,sommet);
    eblist_ins_tete(&(list_vvs),sommet);
  }
//printf("ici2!!!\n");
  //ebvor_vv_init_label(poly->vg);
//printf("ici3!!!\n");
  self->function->vectorCur=self->function->vectorGlob;//printf("ici3!!!\n");
//printf("iciii222!!!\n");
  englob=ebfunc_termDel3_function_componentList(self,list_vvs,poly);

  return englob;
}
//////////////////////////////////////////////////////Func component list side!!!
void ebfunc_termDel3_update_component(FUNC_TERM_DEL3 self,CQLS_VECTOR_LIST vectorCache,PT_POLY poly) {
  
  ebcomponent_termDel3_updateInfo(self->component,poly);
  cqlsVectorList_insert(vectorCache,cqlsVector_from_envR_to_new_vector_after_update(self->function->vectorCur,EBFUNC_R));
}

void ebfunc_termDel3_update_componentList(FUNC_TERM_DEL3 self,CQLS_VECTOR_LIST vectorCache,PT_LIST pt_list,PT_POLY poly) {
  ////PARTIE COMMUNE
  PT_LIST pt;
  PT_VV sommet,voisin;
  int i,i1,i2;

  //printf("list is NULL=%d\n",(pt_list==NULL ? 1 : 0));
  ebpoly_init_label(poly);
 
  //parcourrir les ar�es
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
  sommet=(PT_VV)pt->pt_cur;
  ////PARTIE MODIFIABLE SELON FCT INTERACTION
  //points de l'ar�e dans la direction i!!!
  ebcomponent_termDel3_set(self->component,sommet);
  ebfunc_termDel3_update_component(self,vectorCache,poly);
      ////FIN PARTIE MODIFIABLE
  }
  ////FIN PARTIE COMMUNE
}

//SEULE INTERFACE UTILSEE PAR FUNC EXPO!!!
void ebfunc_termDel3_componentLocalUpdate(FUNC_TERM_DEL3 self,PT_POLY poly) {
 CQLS_VECTOR_LIST vectorCache;

  self->function->vectorCur=self->function->vectorLoc;
  ebpoly_apply(poly);
  //printf("poly->mode=%d\n",poly->mode);
  if (poly->mode==1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;
  ebfunc_termDel3_update_componentList(self,vectorCache,poly->list_vvs_nouveaux,poly); 


  ebpoly_cancel(poly);
  if (poly->mode==-1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;

  ebfunc_termDel3_update_componentList(self,vectorCache,poly->list_vvs_supprimes,poly); 

}

void ebfunc_termDel3_componentGlobalUpdate(FUNC_TERM_DEL3 self,PT_POLY poly) {
  int i;
  PT_VV sommet;
  PT_LIST list_vvs=NULL;
  CQLS_VECTOR_LIST vectorCache;
  
//printf("Del2GU!!!!\n");
  for(i=0;i<poly->vg->nb_vv;i++) {
    sommet = (PT_VV)(poly->vg->tab_vv+i);
    if(ebvv_isDispo(sommet)) continue;
//printf("sommet[%d]=%d\n",i,sommet);
    eblist_ins_tete(&(list_vvs),sommet);
  }
//printf("Del3GU2!!!\n");
  ebvor_all_vvs_init_label(poly->vg);
//printf("Del3U3!!!\n");
  self->function->vectorCur=self->function->vectorGlob;//printf("ici3!!!\n");
  vectorCache=self->function->cacheGlob->list;//printf("ici3!!!\n");
//printf("Del3GU4!!!->%d\n",vectorCache);
  ebfunc_termDel3_update_componentList(self,vectorCache,list_vvs,poly);
}


/****************/
/*    All2      */
/****************/
/////////////////////////// Component termAll2
COMPONENT_TERM_ALL2 ebcomponent_termAll2_new() {
  COMPONENT_TERM_ALL2 self;

  self=(COMPONENT_TERM_ALL2)Calloc(1,ST_COMPONENT_TERM_ALL2);
  self->info=ebfunc_info_new(INFO_ALL2_LAST);
  return self;
}

void ebcomponent_termAll2_free(COMPONENT_TERM_ALL2 self) {
 if (self!=(COMPONENT_TERM_ALL2)NULL) {
    ebfunc_info_free(self->info);
    Free(self);
  }
}


void ebcomponent_termAll2_set(COMPONENT_TERM_ALL2 self,PT_DV point1,PT_DV point2) {
  self->point1=point1;
  self->point2=point2;
}


//RMK: NOW NO USE OF SELF->INFOS!!!!
void ebcomponent_termAll2_updateInfo(COMPONENT_TERM_ALL2 self,PT_POLY poly) {
  int i,ii,infoId,nbRes,id[2];
  DOUBLE res,*res2;
  SEXP resR,tmpR;
  PT_DV point1=self->point1,point2=self->point2;

  //printf("updateInfo All2\n");
  for(i=0;i<self->info->nbInfoList;i++) {
    infoId=self->info->infoList[i];
    switch(infoId) {
      case INFO_ALL2_ID: {
        //to check! Never used
        id[0]=(int)(point1-poly->vg->tab_dv);id[1]=(int)(point2-poly->vg->tab_dv);
#ifdef debugModeR
        Rprintf("point1=%d,point2=%d\n",id[0],id[1]);
#endif
        cqlsR_INT2SEXP(EBFUNC_R,INFO_KEY_ALL2[infoId],id,2);
	      break;
      }
      case INFO_ALL2_X: {//Vector of Coordinates
        PROTECT(resR=allocVector(VECSXP,2));//resR<-list() with 2 elements 
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
        for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point1)->point[ii]);
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
	      for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point2)->point[ii]);
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_ALL2[infoId]),resR,EBFUNC_R->env);// assign("v",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      }
      case INFO_ALL2_V: { //marks points
	      PROTECT(resR=allocVector(VECSXP,2));//resR<-list() with 2 elements 
	      DC_curObj(poly->vg->dc,*point1);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->dc->n));//tmpR <- list()
	      cqlsSEXP_setAllVars_List(tmpR,poly->vg->dc);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      DC_curObj(poly->vg->dc,*point2);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->dc->n));//tmpR <- list()
	      cqlsSEXP_setAllVars_List(tmpR,poly->vg->dc);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_ALL2[infoId]),resR,EBFUNC_R->env);// assign("v",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      } 
      case INFO_ALL2_L2: {
	      //length edge
	      res=ebcalcul_distance((*point1)->point,(*point2)->point);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_ALL2[infoId],&res,1);
	      //printf("l2=%LF\n",res);
	      break;
      }
      case INFO_ALL2_L: {
	      //length edge
	      res=sqrt(ebcalcul_distance((*point1)->point,(*point2)->point));
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_ALL2[infoId],&res,1);
	      //printf("l2=%LF\n",res);
	      break;
      }
    }
  }
}
/////////////////////////// Func termAll2
FUNC_TERM_ALL2 ebfunc_termAll2_new(FUNCTION function) {
  FUNC_TERM_ALL2 self;
  self=(FUNC_TERM_ALL2)Calloc(1,ST_FUNC_TERM_ALL2);
  self->component=ebcomponent_termAll2_new();
  self->info=self->component->info;
  self->function=function;
  self->range=(DOUBLE)REAL(ebfunc_eval1("if(is.null(term$args$range)) -1 else term$args$range"))[0];
#ifdef debugModeR
  Rprintf("All2 range=%LF\n",self->range);
#endif
  ebfunc_info_add(self->info, function->infoList, function->nbInfoList,0);
  return self;
}

void ebfunc_termAll2_free(FUNC_TERM_ALL2 self) {
  if (self!=(FUNC_TERM_ALL2)NULL) {
    ebcomponent_termAll2_free(self->component);
    Free(self);
  }
}

//////////////////////////////////////////////////////Func non expo side!!!
DOUBLE ebfunc_termAll2_function_component(FUNC_TERM_ALL2 self,PT_POLY poly) {
  DOUBLE enloc=0.0;
  int i;
  
  ebcomponent_termAll2_updateInfo(self->component,poly);
    cqlsVector_eval(self->function->vectorCur,EBFUNC_R);//printf("ici2\n");
    cqlsRExpr_eval(self->function->R);   //printf("ici3\n");
    enloc = cqlsSEXP_DBL(self->function->R->ans,0);//printf("ici4\n");
  return enloc;
}

DOUBLE ebfunc_termAll2_function_componentList(FUNC_TERM_ALL2 self,PT_POLY poly) {
  ////PARTIE COMMUNE  ENERGIE PAIRE LIST
  PT_DV pt_p; 
  PT_LIST list_dvs=NULL;
  int i;
  //initialisation 
  DOUBLE enloc=0.0; 

  if(self->range <0) {
    for(i=0;i<poly->vg->nb_dv;i++) {
      pt_p=(poly->vg->tab_dv)+i;
      if(*pt_p==NULL) continue;
      if((*pt_p)->num_s==-3) continue;
      if(pt_p==poly->point_courant) continue;
      ebcomponent_termAll2_set(self->component,(PT_DV)poly->point_courant,pt_p);
      enloc += ebfunc_termAll2_function_component(self,poly);
    }
  } else {//range
//Rprintf("Début termAll2\n");
    ebvor_all_dvs_from_dv_at_range(poly->vg,(PT_DV)poly->point_courant,self->range,&list_dvs);//Rprintf("Fin all points\n");
    while(list_dvs!=NULL) {
      pt_p=(PT_DV)eblist_recup_tete(&list_dvs);
      //Rprintf("Dans list_dvs: %p\n",pt_p);
      ebcomponent_termAll2_set(self->component,(PT_DV)poly->point_courant,pt_p);
      enloc += ebfunc_termAll2_function_component(self,poly);
    }
//Rprintf("Fin enloc=%LF\n",enloc);
  }
  return enloc;
  ////FIN PARTIE COMMUNE
}

////SEULE INTERFACE AU NIVEAU ENERGIE!!!
DOUBLE ebfunc_termAll2_localEnergy(FUNC_TERM_ALL2 self, PT_POLY poly) {
  DOUBLE enloc=0.0;

  self->function->vectorCur=self->function->vectorLoc;
  //printf("localEnergy All2\n");
  ebpoly_apply(poly);
  enloc += ebfunc_termAll2_function_componentList(self,poly);
  ebpoly_cancel(poly);
  //No negative contribution
  //enloc*=(DOUBLE)poly->mode;
  
  return enloc;
}

DOUBLE ebfunc_termAll2_globalEnergy(FUNC_TERM_ALL2 self, PT_POLY poly) {
  DOUBLE enloc=0.0;
  
  return enloc;
}
//////////////////////////////////////////////////////Func expo side!!!
void ebfunc_termAll2_update_component(FUNC_TERM_ALL2 self,CQLS_VECTOR_LIST vectorCache,PT_POLY poly) { 
 
  ebcomponent_termAll2_updateInfo(self->component,poly);
  cqlsVectorList_insert(vectorCache,cqlsVector_from_envR_to_new_vector_after_update(self->function->vectorCur,EBFUNC_R)); 
}

void ebfunc_termAll2_update_componentList(FUNC_TERM_ALL2 self,CQLS_VECTOR_LIST vectorCache, PT_POLY poly) {
  PT_DV pt_p;
  PT_LIST list_dvs=NULL;
  int i;

  if(self->range <0) {
    //printf("nb_dv=%d\n",poly->vg->nb_dv);
    for(i=0;i<poly->vg->nb_dv;i++) {
      pt_p=(poly->vg->tab_dv)+i; 
      //printf("i=%d,pt_p=%d,*pt_p=%d\n",i,pt_p,*pt_p);
      if(*pt_p==NULL) continue;
      //printf("pt_p->num_s=%d\n",(*pt_p)->num_s);
      if((*pt_p)->num_s==-3) continue;
      //printf("courant=%d\n",poly->point_courant);
      if(pt_p==poly->point_courant) continue;
      ebcomponent_termAll2_set(self->component,(PT_DV)poly->point_courant,pt_p);
      ebfunc_termAll2_update_component(self,vectorCache,poly);
    }
  } else {
    ebvor_all_dvs_from_dv_at_range(poly->vg,(PT_DV)poly->point_courant,self->range,&list_dvs);
    while(list_dvs!=NULL) {
      pt_p=(PT_DV)eblist_recup_tete(&list_dvs);
      ebcomponent_termAll2_set(self->component,(PT_DV)poly->point_courant,pt_p);
      ebfunc_termAll2_update_component(self,vectorCache,poly);
    }
  }
  ////FIN PARTIE COMMUNE
}

//SEULE INTERFACE UTILSEE PAR FUNC!!!
void ebfunc_termAll2_componentLocalUpdate(FUNC_TERM_ALL2 self,PT_POLY poly) {
  
  self->function->vectorCur=self->function->vectorLoc;
  //printf("localEnergy All2\n");
  ebpoly_apply(poly);
  ebfunc_termAll2_update_componentList(self,self->function->cacheLoc->list,poly); //en insertion
  ebpoly_cancel(poly);
  //No negative contribution
  //enloc*=(DOUBLE)poly->mode;

}

void ebfunc_termAll2_componentGlobalUpdate(FUNC_TERM_ALL2 self,PT_POLY poly) {
 
 
}


/****************/
/*    NNG       */
/****************/
/////////////////////////// Component termNNG
COMPONENT_TERM_NNG ebcomponent_termNNG_new() {
  COMPONENT_TERM_NNG self;

  self=(COMPONENT_TERM_NNG)Calloc(1,ST_COMPONENT_TERM_NNG);
  self->info=ebfunc_info_new(INFO_NNG_LAST);
  return self;
}

void ebcomponent_termNNG_free(COMPONENT_TERM_NNG self) {
 if (self!=(COMPONENT_TERM_NNG)NULL) {
    ebfunc_info_free(self->info);
    Free(self);
  }
}


void ebcomponent_termNNG_set(COMPONENT_TERM_NNG self,PT_DV point1,PT_DV point2,PT_VV sommet,PT_VV sommetv) {
  self->point1=point1;
  self->point2=point2;
  //dual edge (used for length ortho edge)
  self->sommet=sommet;
  self->sommetv=sommetv;
}

void ebcomponent_termNNG_setFromSommet(COMPONENT_TERM_NNG self,PT_VV sommet,int side) {
int i1,i2;  
  self->sommet=sommet;
  ebutil_indices_arete(side,&i1,&i2);
  self->point1=(*sommet)->ndvs[i1];
  self->point2=(*sommet)->ndvs[i2];
  //dual edge (used for length ortho edge)
  self->sommetv=(*sommet)->nvvs[side];
}

int ebcomponent_termNNG_unproper(COMPONENT_TERM_NNG self,PT_POLY poly) {
return (self->point1-poly->vg->tab_dv<0 | self->point2-poly->vg->tab_dv<0);
}

//RMK: NOW NO USE OF SELF->INFOS!!!!
void ebcomponent_termNNG_updateInfo(COMPONENT_TERM_NNG self,PT_POLY poly) {
  int i,ii,infoId,nbRes,id[2];
  DOUBLE res,*res2;
  SEXP resR,tmpR;
  PT_DV point1=self->point1,point2=self->point2;

  //printf("updateInfo NNG:nbInfo=%d\n",self->info->nbInfoList);
  //for(i=0;i<self->info->nbInfoList;i++) Rprintf("InfoId=%d\n",self->info->infoList[i]);

  for(i=0;i<self->info->nbInfoList;i++) {
    infoId=self->info->infoList[i];
    //printf("Update NNG -> InfoId=%d\n",infoId);
    switch(infoId) {
      case INFO_NNG_ID: {
        //to check! Never used
        id[0]=(int)(point1-poly->vg->tab_dv);id[1]=(int)(point2-poly->vg->tab_dv);
#ifdef debugModeR
        Rprintf("point1=%d,point2=%d\n",id[0],id[1]);
#endif
        cqlsR_INT2SEXP(EBFUNC_R,INFO_KEY_NNG[infoId],id,2);
	      break;
      }
      case INFO_NNG_X: {//Vector of Coordinates
        PROTECT(resR=allocVector(VECSXP,2));//resR<-list() with 2 elements 
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
        for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point1)->point[ii]);
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      PROTECT(tmpR=allocVector(REALSXP,2));//tmpR <- numeric(2)
	      for(ii=0;ii<2;ii++) REAL(tmpR)[ii]=(DOUBLE)((*point2)->point[ii]);
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_NNG[infoId]),resR,EBFUNC_R->env);// assign("v",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      }
      case INFO_NNG_V: { //marks points
	      PROTECT(resR=allocVector(VECSXP,2));//resR<-list() with 2 elements 
	      DC_curObj(poly->vg->dc,*point1);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->marks->nbData));//tmpR <- list()
	      DU_cqlsSEXP_setAllVars_List(tmpR,poly->vg->marks);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,0,tmpR);//resR[[1]]<-tmpR
	      UNPROTECT(1);//tmpR
	      DC_curObj(poly->vg->dc,*point2);
	      PROTECT(tmpR=allocVector(VECSXP,poly->vg->marks->nbData));//tmpR <- list()
	      DU_cqlsSEXP_setAllVars_List(tmpR,poly->vg->marks);//tmpR is set here from dc
	      SET_VECTOR_ELT(resR,1,tmpR);//resR[[2]]<-tmpR
	      UNPROTECT(1);//tmpR
	      defineVar(install(INFO_KEY_NNG[infoId]),resR,EBFUNC_R->env);// assign("v",resR,.funcEnv)
	      UNPROTECT(1);//resR
	      break;
      }
      case INFO_NNG_A: {//area point1-> do not use now!!!
	      res2=(DOUBLE*)Calloc(2,DOUBLE);
	      res2[0]=ebdv_area(point1,poly);
	      //printf("area[0]=%LF\n",res2[0]);
	      res2[1]=ebdv_area(point2,poly);
	      //printf("area[1]=%LF\n",res2[1]);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_NNG[infoId],res2,2);
	      Free(res2);
	      break;
      }
      case INFO_NNG_L2: {
	      //length edge
	      res=ebcalcul_distance((*point1)->point,(*point2)->point);
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_NNG[infoId],&res,1);
	      //printf("l2=%LF\n",res);
	      break;
      }
      case INFO_NNG_L: {
	      //length edge
	      res=sqrt(ebcalcul_distance((*point1)->point,(*point2)->point));
	      cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_NNG[infoId],&res,1);
	      //printf("l=%LF\n",res);
	      break;
      }
      case INFO_NNG_DA: {//Difference of two areas
        res=ebdv_area(point1,poly);
        //Rprintf("A(point1=%LF)-A(point2=%LF)",res,ebdv_area(point2,poly));
	      res=fabs(res-ebdv_area(point2,poly));
        //printf("da=%LF\n",res);
        cqlsR_DBL2SEXP(EBFUNC_R,INFO_KEY_NNG[infoId],&res,1);
      }
    }
    //printf("OUT\n");
  }
}
/////////////////////////// Func termNNG
FUNC_TERM_NNG ebfunc_termNNG_new(FUNCTION function) {
  FUNC_TERM_NNG self;
  self=(FUNC_TERM_NNG)Calloc(1,ST_FUNC_TERM_NNG);
  self->component=ebcomponent_termNNG_new();
  self->info=self->component->info;
  self->function=function;
  self->order=INTEGER(ebfunc_eval1("as.integer(if(is.null(term$args$order)) 0 else term$args$order)"))[0];//O even if order is actually 1 but with different method! 
#ifdef debugModeR
  Rprintf("NNG order=%d\n",self->order);
#endif
  ebfunc_info_add(self->info, function->infoList, function->nbInfoList,0);
  return self;
}

void ebfunc_termNNG_free(FUNC_TERM_NNG self) {
  if (self!=(FUNC_TERM_NNG)NULL) {
    ebcomponent_termNNG_free(self->component);
    Free(self);
  }
}

//////////////////////////////////////////////////////Func non expo side!!!
DOUBLE ebfunc_termNNG_function_component(FUNC_TERM_NNG self,PT_POLY poly) {
  DOUBLE enloc=0.0;
  int i;
  
  //update the info from poly!
  ebcomponent_termNNG_updateInfo(self->component,poly);//printf("ici\n");
    cqlsVector_eval(self->function->vectorCur,EBFUNC_R);//printf("ici2\n");
    cqlsRExpr_eval(self->function->R);   //printf("ici3\n");
    enloc = cqlsSEXP_DBL(self->function->R->ans,0);//printf("ici4\n");
  return enloc;
}

DOUBLE ebfunc_termNNG_function_componentList(FUNC_TERM_NNG self, PT_LIST pt_list,PT_POLY poly) {
  ////PARTIE COMMUNE  ENERGIE PAIRE LIST
  PT_LIST pt,nvvsList=NULL;
  PT_VV vv,nvv; 
  int i;
  //initialisation 
  DOUBLE enloc=0.0; 

  //init nvvsList
  if(self->order==0) {//in fact order 1 too
    //remettre les labels à 0!
    //ebpoly_init_label(poly); //TODO: normally, should be done at the end and not at the begininng!
    nvvsList=pt_list; //ATTENTION: this is not a copy so no deletion
  } else {
    ebvor_nvvs_for_vvList_until_order(poly->vg,pt_list,self->order,&nvvsList);
  }

  //run over nvvsList elements
  for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {
    vv=(PT_VV)pt->pt_cur;
    for(i=0;i<3;i++) {
      nvv=(*vv)->nvvs[i];
      if(nvv==NULL) continue;
      if((*nvv)->ind != 0) continue;
      ////PARTIE MODIFIABLE SELON FCT INTERACTION
      //points de l'arête dans la direction i!!!
      ebcomponent_termNNG_setFromSommet(self->component,vv,i);
//#ifdef TEST_SUP
      //printf("paire[(%LF,%LF)(%LF,%LF)]=%LF\n",(*point1)->point[0],(*point1)->point[1],(*point2)->point[0],(*point2)->point[1],ebsim_straussDelEdge_eval(self,(*point1)->point,(*point2)->point));
//#endif
      if(ebcomponent_termNNG_unproper(self->component,poly)) continue;
      enloc += ebfunc_termNNG_function_component(self,poly);
      ////FIN PARTIE MODIFIABLE
    }
    (*vv)->ind=1;
  }
  
  //clean label to be polite!
  if(self->order==0) //do not destroy nvvsList which is pt_list here!
    for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {vv=(PT_VV)pt->pt_cur;(*vv)->ind=0;}
  else 
    while(nvvsList!=NULL) {vv=(PT_VV)eblist_recup_tete(&nvvsList);(*vv)->ind=0;}
     
  return enloc;
  ////FIN PARTIE COMMUNE
}

////SEULE INTERFACE AU NIVEAU ENERGIE!!!
DOUBLE ebfunc_termNNG_localEnergy(FUNC_TERM_NNG self, PT_POLY poly) {
  DOUBLE enloc=0.0;

  self->function->vectorCur=self->function->vectorLoc;
  ebpoly_apply(poly);
  enloc += ebfunc_termNNG_function_componentList(self,poly->list_vvs_nouveaux,poly);
  ebpoly_cancel(poly);
  enloc -= ebfunc_termNNG_function_componentList(self,poly->list_vvs_supprimes,poly);
  
  enloc*=(DOUBLE)poly->mode;
  
  return enloc;
}

DOUBLE ebfunc_termNNG_globalEnergy(FUNC_TERM_NNG self, PT_POLY poly) {
  DOUBLE enloc=0.0;
  int i;
  PT_VV sommet;
  PT_LIST list_vvs=NULL;
 
   for(i=0;i<poly->vg->nb_vv;i++) {
    sommet = (PT_VV)(poly->vg->tab_vv+i);
    if(ebvv_isDispo(sommet)) continue;
//printf("sommet[%d]=%d\n",i,sommet);
    eblist_ins_tete(&(list_vvs),sommet);
  }
//printf("ici2!!!\n");
  //ebvor_vv_init_label(poly->vg);
//printf("ici3!!!\n");
  self->function->vectorCur=self->function->vectorGlob;//printf("ici3!!!\n");
//printf("iciii222!!!\n");
  enloc=ebfunc_termNNG_function_componentList(self,list_vvs,poly);

  return enloc;
}
//////////////////////////////////////////////////////Func component list side!!!
void ebfunc_termNNG_update_component(FUNC_TERM_NNG self,CQLS_VECTOR_LIST vectorCache, PT_POLY poly) {
  
  ebcomponent_termNNG_updateInfo(self->component,poly);
  cqlsVectorList_insert(vectorCache,cqlsVector_from_envR_to_new_vector_after_update(self->function->vectorCur,EBFUNC_R));
}

void ebfunc_termNNG_update_componentList(FUNC_TERM_NNG self,CQLS_VECTOR_LIST vectorCache,PT_LIST pt_list, PT_POLY poly) {
  ////PARTIE COMMUNE
  PT_LIST pt,nvvsList;
  PT_VV vv,nvv;
  int i;


/*OLD
  //printf("list is NULL=%d\n",(pt_list==NULL ? 1 : 0));
  //mettre les labels à 0!
  //ebpoly_init_label(poly); => PUT IN componentUpdate TO BE USEABLE FOT NNGGLOB!
  //parcourrir les arêtes
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
  sommet=(PT_VV)pt->pt_cur;
//printf("debug: sommet-poly->vg->tab_vv=%d\n",sommet-poly->vg->tab_vv);
  //if(sommet-poly->vg->tab_vv<0) continue;
#ifdef DEBUG_PSEUDO_COMPONENT_EDGE
  Rprintf("sommet=%d\n",*sommet);
#endif
  for(i=0;i<3;i++) {
    voisin=(*sommet)->nvvs[i];
#ifdef DEBUG_PSEUDO_COMPONENT_EDGE
    Rprintf("voisin=%d\n",*voisin);
#endif
    if(*voisin==NULL) continue;
#ifdef DEBUG_PSEUDO_COMPONENT_EDGE
      Rprintf("voisin->label=%d\n",(*voisin)->label);
#endif
    if((*voisin)->label != 0) continue;
      ////PARTIE MODIFIABLE SELON FCT INTERACTION
      //points de l'arête dans la direction i!!!
      ebcomponent_termNNG_setFromSommet(self->component,sommet,i);
      if(ebcomponent_termNNG_unproper(self->component,poly)) continue;
      ebfunc_termNNG_update_component(self,vectorCache,poly);
      ////FIN PARTIE MODIFIABLE
  }
  (*sommet)->label=1;
*/

  //init nvvsList
  if(self->order==0) {//in fact order 1 too
    nvvsList=pt_list; //ATTENTION: this is not a copy so no deletion
  } else {
    ebvor_nvvs_for_vvList_until_order(poly->vg,pt_list,self->order,&nvvsList);
  }

  //run over nvvsList elements
  for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {
    vv=(PT_VV)pt->pt_cur;
    for(i=0;i<3;i++) {
      nvv=(*vv)->nvvs[i];
      if(nvv==NULL) continue;
      if((*nvv)->ind != 0) continue;
      ////PARTIE MODIFIABLE SELON FCT INTERACTION
      //points de l'arête dans la direction i!!!
      ebcomponent_termNNG_setFromSommet(self->component,vv,i);
//#ifdef TEST_SUP
      //printf("paire[(%LF,%LF)(%LF,%LF)]=%LF\n",(*point1)->point[0],(*point1)->point[1],(*point2)->point[0],(*point2)->point[1],ebsim_straussDelEdge_eval(self,(*point1)->point,(*point2)->point));
//#endif
      if(ebcomponent_termNNG_unproper(self->component,poly)) continue;
      ebfunc_termNNG_update_component(self,vectorCache,poly);
      ////FIN PARTIE MODIFIABLE
    }
    (*vv)->ind=1;
  }
  //clean label to be polite!
  if(self->order==0) 
    for(pt=nvvsList;pt!=NULL;pt=pt->pt_next) {vv=(PT_VV)pt->pt_cur;(*vv)->ind=0;}
  else 
    while(nvvsList!=NULL) {vv=(PT_VV)eblist_recup_tete(&nvvsList);(*vv)->ind=0;}
     
  ////FIN PARTIE COMMUNE
}

//SEULE INTERFACE UTILSEE PAR FUNC COMPONENT!!!
void ebfunc_termNNG_componentLocalUpdate(FUNC_TERM_NNG self,PT_POLY poly) {
CQLS_VECTOR_LIST vectorCache;

  self->function->vectorCur=self->function->vectorLoc;  
  ebpoly_apply(poly);
  //printf("poly->mode=%d\n",poly->mode);
  if (poly->mode==1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;
  ebpoly_init_label(poly); //mettre les labels à 0!
  ebfunc_termNNG_update_componentList(self,vectorCache,poly->list_vvs_nouveaux,poly); 


  ebpoly_cancel(poly);
  if (poly->mode==-1) vectorCache=self->function->cacheLoc->list; else vectorCache=self->function->cacheLoc->oldList;
  ebpoly_init_label(poly);//mettre les labels à 0!
  ebfunc_termNNG_update_componentList(self,vectorCache,poly->list_vvs_supprimes,poly); 
}

/***************/
/* NNG Global */
/***************/
//same thing as ebfunc_termNNG_componentUpdate execpt that the list is bigger!
//and there is no negative contribution!

void ebfunc_termNNG_componentGlobalUpdate(FUNC_TERM_NNG self,PT_POLY poly) {
  int i;
  PT_VV sommet;
  PT_LIST list_vvs=NULL;
  CQLS_VECTOR_LIST vectorCache;
  
//printf("NNGGU!!!!\n");
  for(i=0;i<poly->vg->nb_vv;i++) {
    sommet = (PT_VV)(poly->vg->tab_vv+i);
    if(ebvv_isDispo(sommet)) continue;
//printf("sommet[%d]=%d\n",i,sommet);
    eblist_ins_tete(&(list_vvs),sommet);
  }
//printf("NNGGU2!!!\n");
  ebvor_all_vvs_init_label(poly->vg);
//printf("NNGU3!!!\n");
  self->function->vectorCur=self->function->vectorGlob;//printf("ici3!!!\n");
  vectorCache=self->function->cacheGlob->list;//printf("ici3!!!\n");
//printf("NNGGU4!!!->%d\n",vectorCache);
  ebfunc_termNNG_update_componentList(self,vectorCache,list_vvs,poly);
//printf("globUpdate\n");cqlsVectorList_print(vectorCache); 
}


