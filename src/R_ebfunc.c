
#include "ebvor.h"
#include "ebfunc.h"
#include "ebcache.h"
#include <stdlib.h>
#include <math.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/PrtUtil.h>

//#ifndef Win32
//#include <R_ext/eventloop.h>
//#endif
#define debugMode_OFF

#include "R_ebspat.h"

//Class EBFunc
void C_ebfunc_free(SEXP self) {
FUNC func;
    
  func=(FUNC)R_ExternalPtrAddr(self);
  if(func==NULL) return;
#ifdef debugMode
  Rprintf("ebfunc %p removed!\n",func);
#endif
  ebfunc_free(func);
  R_ClearExternalPtr(self);
}

SEXP R_ebfunc_free(SEXP args) {
  C_ebfunc_free(CADR(args));
  return R_NilValue;
}

SEXP R_ebfunc_new(SEXP args) {
  FUNC func;
  SEXP self;
//Rprintf("R_ebfunc_new\n");
  //CQLSR_within_R(1);//means that this is executed in R!!!
  func=ebfunc_new();
//Rprintf("nbTermList=%d\n",func->nbTermList);
  self=R_MakeExternalPtr(func, R_NilValue, R_NilValue);
  PROTECT(self);
  //R_RegisterCFinalizerEx(self,(R_CFinalizer_t)C_ebfunc_free, TRUE);
  UNPROTECT(1);
  return self;
}

void* C_ebfunc_reactivate(SEXP funcR) {
  int i;
  SEXP tmp,ans,cmd,newTerms,modeR,fctR,functionR;
  FUNC func;
  FUNCTION function;
  char cmdStr[256];
  
  //CQLSR_within_R(1);//means that this is executed in R!!!
  func=ebfunc_new();
#ifdef debugMode
  Rprintf("Reactivating ebfunc ...\n");
#endif
  //Single
  /*PROTECT(tmp=coerceVector(getAttrib(funcR,install("Single")),REALSXP));
  if(REAL(tmp)!=NULL) {
    ebfunc_add_single(func);
    sprintf(cmdStr,"Single<-%lf",REAL(tmp)[0]);
	//printf("cmdStr=%s\n",cmdStr);
    ebfunc_eval1(cmdStr);
  }
  UNPROTECT(1); => No need now since it is attached to the Single envir! */
  //terms
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(funcR,"terms"),STRSXP));
  PROTECT(fctR=coerceVector(C_getVarForExternalPtr(funcR,"fct"),VECSXP));
  PROTECT(newTerms=coerceVector(C_getVarForExternalPtr(funcR,"newTerms"),INTSXP));
#ifdef debugMode
  Rprintf("length(tmp)=%d\n",length(tmp));
#endif
  PROTECT(modeR=coerceVector(C_getVarForExternalPtr(funcR,"mode"),STRSXP));
#ifdef debugMode
  Rprintf("mode=%s\n",CHAR(STRING_ELT(modeR,0)));
#endif
  sprintf(cmdStr,"EBFunc.mode(\"%s\")",CHAR(STRING_ELT(modeR,0)));
#ifdef debugMode
  Rprintf("cmd: %s\n",cmdStr);
#endif
  CQLSR_eval1(cmdStr); 
  
  for(i = 0; i < length(tmp); i++) {
    PROTECT(cmd=STRING_ELT(tmp, i));
#ifdef debugMode
    Rprintf("%d,type=%d,val=%s\n",i,TYPEOF(cmd),CHAR(cmd));
#endif
    function=(FUNCTION) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(VECTOR_ELT(fctR,i+1)),C_ebfunc_function_reactivate);
    //PROTECT(ans=ebfunc_eval1(CHAR(cmd)));
    //function=(FUNCTION)R_ExternalPtrAddr(ans);
#ifdef debugMode
    //R_SetExternalPtrAddr(VECTOR_ELT(fctR,i+1), function); //register the new created function to fctR[[i+1]]
    Rprintf("C_ebfunc_reactivate: ebfunc_add_function %p newTerm=%d for func %p\n",function,INTEGER(newTerms)[0],func);
#endif
    ebfunc_add_function(func,function,INTEGER(newTerms)[0]);
    UNPROTECT(1);
  }
  UNPROTECT(4);
#ifdef debugMode
  Rprintf("ebfunc reactivated!\n");
#endif
  return func;
}

SEXP R_ebfunc_reactivate(SEXP args) {
  FUNC func;
  func=(FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebfunc_reactivate);
  return R_NilValue;
}

/*SEXP R_ebfunc_setVor(SEXP args) {//args=vor
  FUNC func;
  PT_VOR vor;
  SEXP funcR,vorR;

  func=(FUNC)R_ExternalPtrAddr(PROTECT(funcR=CADR(args)));
  vor=(PT_VOR)R_ExternalPtrAddr(PROTECT(vorR=CADDR(args)));
  ebfunc_setVor(func,vor);
  //setAttrib(funcR,install("vor"),vorR);
  //Rprintf("inside setVor\n");
  UNPROTECT(2);
  return R_NilValue;
}*/

//TODO to REMOVE!
/*SEXP R_ebfunc_add_single(SEXP args) {
  FUNC func;
  SEXP singleR,funcR,tmp,tmp2;

  PROTECT(funcR=CADR(args));
  func=(FUNC) cqlsR_ExternalPtrAddr(funcR,C_ebfunc_reactivate);
  ebfunc_add_single(func);
  PROTECT(singleR=CADDR(args));
  //printf("Single=%lf\n",REAL(singleR)[0]);
  //Register Single!
  PROTECT(tmp=allocVector(REALSXP,1));
  PROTECT(tmp2=coerceVector(getAttrib(funcR,install("Single")),REALSXP));
  //printf("c sd=%d,%d,%d\n",tmp2,getAttrib(funcR,install("Single")),REAL(tmp2));
  if(REAL(tmp2)==NULL) {
    REAL(tmp)[0]=REAL(singleR)[0];
  } else {
    REAL(tmp)[0]=REAL(tmp2)[0]+REAL(singleR)[0];
  }
  setAttrib(funcR,install("Single"),tmp);
  UNPROTECT(4);
  return R_NilValue;
}*/

void R_ebfunc_attrib_function(SEXP funcR, SEXP functionR) {
  SEXP tmp,tmp2,tmp3;
  int i;

  //register attr(terms)
  PROTECT(tmp=coerceVector(C_getCqlsObjVar(functionR,"call"),STRSXP)); 
  PROTECT(tmp2=coerceVector(C_getCqlsObjVar(funcR,"terms"),STRSXP));
  PROTECT(tmp3=allocVector(STRSXP,length(tmp2)+1));

  for(i=0;i<length(tmp2);i++) {
    SET_STRING_ELT(tmp3,i,mkChar(CHAR(STRING_ELT(tmp2,i))));
  }
  SET_STRING_ELT(tmp3,length(tmp2),mkChar(CHAR(STRING_ELT(tmp,0))));
  C_setCqlsObjVar(funcR,"terms",tmp3);
  UNPROTECT(3);
  //register function term
  PROTECT(tmp2=coerceVector(C_getCqlsObjVar(funcR,"fct"),VECSXP));
  PROTECT(tmp3=allocVector(VECSXP,length(tmp2)+1));
  for(i=0;i<length(tmp2);i++) {
    SET_VECTOR_ELT(tmp3,i,VECTOR_ELT(tmp2,i));
  }
  SET_VECTOR_ELT(tmp3,length(tmp2),functionR);
  C_setCqlsObjVar(funcR,"fct",tmp3);
  UNPROTECT(2);
  /*//register attr(infos)
  PROTECT(tmp=ebfunc_eval1("term"));
  PROTECT(tmp2=coerceVector(getAttrib(funcR,install("infos")),VECSXP));
  PROTECT(tmp3=allocVector(VECSXP,length(tmp2)+1));
  for(i=0;i<length(tmp2);i++) {
    SET_VECTOR_ELT(tmp3,i,VECTOR_ELT(tmp2,i));
  }
  SET_VECTOR_ELT(tmp3,length(tmp2),tmp);
  setAttrib(funcR,install("infos"),tmp3);
  UNPROTECT(3);*/
}

SEXP R_ebfunc_add_function(SEXP args) {
  FUNC func;
  FUNCTION function;
  int newIdTerm;
  SEXP functionR,funcR,tmp,tmp2,tmp3;
  
  PROTECT(funcR=coerceVector(CADR(args),ENVSXP));
  func=(FUNC) cqlsR_ExternalPtrAddr(C_getCqlsObjExternalPtr(funcR),C_ebfunc_reactivate);
  PROTECT(functionR=coerceVector(CADDR(args),ENVSXP));
  function=(FUNCTION) cqlsR_ExternalPtrAddr(C_getCqlsObjExternalPtr(functionR),C_ebfunc_function_reactivate);
  newIdTerm=INTEGER(CADDDR(args))[0];
#ifdef debugMode
  Rprintf("R_ebfunc_add_function %p newTerm=%d for func %p\n",function,newIdTerm,func);
#endif
  ebfunc_add_function(func,function,newIdTerm);
  R_ebfunc_attrib_function(funcR,functionR);
  UNPROTECT(2);
  return R_NilValue;
}

/*SEXP R_ebfunc_append_func(SEXP args) {
  FUNC func,func2;
  SEXP funcR,func2R,tmp,tmp2,tmp3;
  int i,ii;

  PROTECT(funcR=CADR(args));
  func=(FUNC) cqlsR_ExternalPtrAddr(funcR,C_ebfunc_reactivate);
  PROTECT(func2R=CADDR(args));
  func2=(FUNC)cqlsR_ExternalPtrAddr(func2R,C_ebfunc_reactivate);
  ebfunc_append_func(func,func2);
  //register Single
  PROTECT(tmp3=coerceVector(getAttrib(func2R,install("Single")),REALSXP));
  if(REAL(tmp3)!=NULL) {
    //printf("Single=%lf\n",REAL(tmp3)[0]);
    PROTECT(tmp=allocVector(REALSXP,1));
    PROTECT(tmp2=coerceVector(getAttrib(funcR,install("Single")),REALSXP));
    //printf("c sd=%d,%d,%d\n",tmp2,getAttrib(funcR,install("Single")),REAL(tmp2));
    if(REAL(tmp2)==NULL) {
      REAL(tmp)[0]=REAL(tmp3)[0];
    } else {
      REAL(tmp)[0]=REAL(tmp2)[0]+REAL(tmp3)[0];
    }
    setAttrib(funcR,install("Single"),tmp);
    UNPROTECT(2);
  }
  UNPROTECT(1);
  //register attr(terms)
  PROTECT(tmp=coerceVector(getAttrib(func2R,install("terms")),STRSXP));
  PROTECT(tmp2=coerceVector(getAttrib(funcR,install("terms")),STRSXP));
  PROTECT(tmp3=allocVector(STRSXP,length(tmp2)+length(tmp)));
  for(i=0;i<length(tmp2);i++) {
    SET_STRING_ELT(tmp3,i,mkChar(CHAR(STRING_ELT(tmp2,i))));
  }
  for(i=0,ii=length(tmp2);i<length(tmp);i++,ii++) {
    SET_STRING_ELT(tmp3,ii,mkChar(CHAR(STRING_ELT(tmp,i))));
  }
  setAttrib(funcR,install("terms"),tmp3);
  UNPROTECT(3);
  //register attr(fct)
  PROTECT(tmp=coerceVector(getAttrib(func2R,install("fct")),VECSXP));
  PROTECT(tmp2=coerceVector(getAttrib(funcR,install("fct")),VECSXP));
  PROTECT(tmp3=allocVector(VECSXP,length(tmp2)+length(tmp)));
  for(i=0;i<length(tmp2);i++) {
    SET_VECTOR_ELT(tmp3,i,VECTOR_ELT(tmp2,i));
  }
  for(i=0,ii=length(tmp2);i<length(tmp);i++,ii++) {
    SET_VECTOR_ELT(tmp3,ii,VECTOR_ELT(tmp,i));
  }
  setAttrib(funcR,install("fct"),tmp3);
  UNPROTECT(3);
  UNPROTECT(2);
  return R_NilValue;
}*/

//Mainly used for internal purpose!
SEXP R_ebfunc_function_append(SEXP args) {
  FUNCTION fct;
  FUNC func;
  int from;

  fct=(FUNCTION) R_ExternalPtrAddr(CADR(args));
  func=(FUNC) cqlsR_ExternalPtrAddr(CADDR(args),C_ebfunc_reactivate);
  from=fct->nbInfoList; //nbInfoList before adding
  ebfunc_function_addInfo(fct);
  //it is not finished! Completed by updating terms[i]
  ebfunc_info_add((FUNC_TERM_)(func->terms[fct->id]->term)->info,fct->infoList,fct->nbInfoList,from); 
  ebfunc_function_addVector(fct);
  return R_NilValue;
}


SEXP R_ebfunc_idTerms(SEXP args) {
  FUNC func;
  SEXP idR;
  int idTerm,i;

  func=(FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebfunc_reactivate);
  
  PROTECT(idR=allocVector(INTSXP,func->nbTermList));
  for(i=0;i<func->nbTermList;i++) {
    idTerm=func->termList[i];
    INTEGER(idR)[i]=idTerm;
  }
  UNPROTECT(1);
  return idR;
}

SEXP R_ebfunc_show_functionList(SEXP args) {
  FUNC func;
  
  func=(FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebfunc_reactivate);
  ebfunc_show_functionList(func);
  return R_NilValue;
}


SEXP R_ebfunc_localEnergy(SEXP args) {
  FUNC func;
  PT_POLY poly;
  SEXP resR;

  func=(FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebfunc_reactivate);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADDR(args),C_ebpoly_reactivate);
  //Rprintf("localEnergy=%LF\n",ebfunc_localEnergy(func,poly));
  PROTECT(resR=allocVector(REALSXP,1));
  REAL(resR)[0]=ebfunc_localEnergy(func,poly);
  UNPROTECT(1);
  return resR;
}

SEXP R_ebfunc_globalEnergy(SEXP args) {
  FUNC func;
  PT_POLY poly;
  SEXP resR;
  
  func=(FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebfunc_reactivate);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADDR(args),C_ebpoly_reactivate);
  //printf("localEnergy=%LF\n",ebfunc_localEnergy(func,poly));
  PROTECT(resR=allocVector(REALSXP,1));
  REAL(resR)[0]=ebfunc_globalEnergy(func,poly);
  UNPROTECT(1);
  return resR;
}

SEXP R_ebfunc_componentShow(SEXP args) {
  FUNC func;
  PT_POLY poly;
  int i;
  
  func=(FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebfunc_reactivate);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADDR(args),C_ebpoly_reactivate);
  i=INTEGER(CADDDR(args))[0];
  //Rprintf("componentCacheNew!\n");
  ebfunc_componentCacheNew(func,0);
  //Rprintf("componentCacheUpdate!\n");
  ebfunc_componentCacheUpdate(func,poly);
  //Rprintf("componentCacheShow!\n");
  ebfunc_componentCacheShow(func,i);
  //Rprintf("componentCacheFree!\n");
  ebfunc_componentCacheFree(func);
  //Rprintf("Bye!!!\n");
  return R_NilValue;
}

SEXP R_ebfunc_componentGet(SEXP args) {
  FUNC func;
  PT_POLY poly;
  int i,local;
  SEXP resR,nameR,tmpR;
  
  func=(FUNC) cqlsR_ExternalPtrAddr(CADR(args),C_ebfunc_reactivate);
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADDR(args),C_ebpoly_reactivate);
  i=INTEGER(CADDDR(args))[0];
  local=INTEGER(CAD4R(args))[0];
  PROTECT(resR=allocVector(VECSXP,2));
  PROTECT(nameR=allocVector(STRSXP,2));
  //printf("componentCacheNew!\n");
  ebfunc_componentCacheNew(func,local);
  //printf("componentCacheUpdate!\n");
  ebfunc_componentCacheUpdate(func,poly);
  //printf("componentCacheShow!\n");
  tmpR=ebfunc_componentCacheGet(func,i,local);
  SET_VECTOR_ELT(resR,0,tmpR);
  SET_STRING_ELT(nameR,0,mkChar("comp"));
  tmpR=ebfunc_componentCacheGetFunc(func,i,local);
  SET_VECTOR_ELT(resR,1,tmpR); 
  SET_STRING_ELT(nameR,1,mkChar("cumComp"));
  setAttrib(resR,R_NamesSymbol,nameR);
  //printf("componentCacheFree!\n");
  ebfunc_componentCacheFree(func);
  //printf("Bye!!!\n");
  UNPROTECT(2);
  return resR;
}

//Class function
SEXP R_ebfunc_function_new(SEXP args) {
  FUNCTION function;
  SEXP self,idR;
  
  PROTECT(idR=coerceVector(CADR(args),INTSXP));
  //PROTECT(callR=CADDR(args));
  function=ebfunc_function_new((FUNC_TYPE)(INTEGER(idR)[0]));//,CHAR(STRING_ELT(callR,0)));
  self=R_MakeExternalPtr(function, R_NilValue, R_NilValue);
  UNPROTECT(1);
  return self;
}


void* C_ebfunc_function_reactivate(SEXP functionR) {
  FUNCTION function;
  int i;
  SEXP tmp,ans;

#ifdef debugMode  
  Rprintf("function to reactivate!\n");
#endif
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(functionR,"call"),STRSXP));
#ifdef debugMode
  Rprintf("cmd:%s\n", CHAR(STRING_ELT(tmp, 0)));
#endif
  PROTECT(ans=ebfunc_eval1(CHAR(STRING_ELT(tmp, 0))));
  function=(FUNCTION)R_ExternalPtrAddr(C_getExternalPtrForCqlsObj(ans));
  UNPROTECT(2);
#ifdef debugMode
  Rprintf("function reactivated!\n");
#endif
  return function;
}
