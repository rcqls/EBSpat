
#include "ebvor.h"
#include "ebfunc.h"
#include <stdlib.h>
#include <math.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/PrtUtil.h>

//#ifndef Win32
//#include <R_ext/eventloop.h>
//#endif

#include "R_ebspat.h"


//C functions used by interface wrapper functions
/*void C_ebsim_genMarks_set(SIM sim, SEXP simR) {
  SEXP genMarksR;
  
  PROTECT(genMarksR=coerceVector(C_getVarForExternalPtr(simR,"del.marks.gen"),STRSXP));
  if(genMarksR != R_NilValue && STRING_ELT(genMarksR,0)!=R_NilValue) {
    if(sim->genMarks==NULL) {
      sim->genMarks=cqlsRExpr_new();
      cqlsRExpr_setEnv(sim->genMarks,EBFuncEnvName);
    }
    Rprintf("genMarks=%s\n",CHAR(STRING_ELT(genMarksR,0)));
    cqlsRExpr_parse(sim->genMarks,CHAR(STRING_ELT(genMarksR,0)));
  }
  UNPROTECT(1);
}*/


//Class EBSim
 SEXP R_ebsim_new(SEXP args) { //args=func,poly,m) {
   SEXP self,m;
   PT_POLY poly;
   FUNC func;
   SIM sim;
   
   func=(FUNC) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CADR(args)),C_ebfunc_reactivate);
   poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(CADDR(args)),C_ebpoly_reactivate);
   PROTECT(m=coerceVector(CADDDR(args),INTSXP));
   //Rprintf("m=%d\n",INTEGER(m)[0]);
   sim=ebsim_new(func,poly,INTEGER(m)[0]);
   self=R_MakeExternalPtr(sim, R_NilValue, R_NilValue);
   UNPROTECT(1);
   return self;
 }

SEXP R_ebsim_free(SEXP args) {
  SIM sim;
  sim=(SIM) R_ExternalPtrAddr(CADR(args));
  if(sim==(SIM)NULL) {
#ifdef debugMode
    Rprintf("EBSim object already freed!\n");
#endif
  } else { 
    ebsim_free(sim);//printf("freed!\n");
    R_SetExternalPtrAddr(CADR(args), (SIM)NULL);
#ifdef debugMode
    Rprintf("EBSim object freed!\n");
#endif
  }
  return R_NilValue;
}


void* C_ebsim_reactivate(SEXP simR) {
  SIM sim;
  FUNC func;
  PT_POLY poly;
  SEXP funcR,polyR,tmp,tmp2;

#ifdef debugMode
  Rprintf("Reactivating ebsim ...\n");
#endif
  PROTECT(funcR=C_getVarForExternalPtr(simR,"func"));
  func = (FUNC) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(funcR),C_ebfunc_reactivate);
  PROTECT(polyR=C_getVarForExternalPtr(simR,"pl"));
  poly=(PT_POLY) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(polyR),C_ebpoly_reactivate);
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(simR,"m"),INTSXP));
  //Rprintf("m=%d\n",INTEGER(tmp)[0]);
  sim=ebsim_new(func,poly,INTEGER(tmp)[0]); 
  UNPROTECT(3);
  //genMarks
  //C_ebsim_genMarks_set(sim,simR);
  //other
  PROTECT(tmp2=coerceVector(C_getVarForExternalPtr(simR,"inside"),INTSXP));
  ebsim_domaineIn_set(sim,INTEGER(tmp2)[0]);
  UNPROTECT(1);
#ifdef debugMode
  Rprintf("ebsim reactivated!\n");
#endif
  return sim; 
}

SEXP R_ebsim_reactivate(SEXP args) {
  SIM sim;
  
  sim=(SIM) cqlsR_ExternalPtrAddr(CADR(args),C_ebsim_reactivate);
  return R_NilValue;
}

/*SEXP R_ebsim_genMarks_set(SEXP args) {
  SIM sim;
  SEXP simR;

  simR=CADR(args);
  sim=(SIM) cqlsR_ExternalPtrAddr(simR,C_ebsim_reactivate);
  C_ebsim_genMarks_set(sim,simR);
}*/

 SEXP R_ebsim_domainIn_set(SEXP args) {
   SIM sim;
   SEXP domaineIn;
   
   sim=(SIM) cqlsR_ExternalPtrAddr(CADR(args),C_ebsim_reactivate);
   PROTECT(domaineIn=coerceVector(CADDR(args),INTSXP));
   //printf("domaineIn=%d\n",INTEGER(domaineIn)[0]);
   ebsim_domaineIn_set(sim,INTEGER(domaineIn)[0]);
   UNPROTECT(1);
   return domaineIn;
 }
 
 SEXP R_ebsim_m_set(SEXP args) {
   SIM sim;
   SEXP m;
  
   sim=(SIM) cqlsR_ExternalPtrAddr(CADR(args),C_ebsim_reactivate);
   PROTECT(m=coerceVector(CADDR(args),INTSXP));
   //Rprintf("m=%d\n",INTEGER(m)[0]);
   sim->m=INTEGER(m)[0];
   UNPROTECT(1);
   return m;
 }


SEXP R_ebsim_func_set(SEXP args) {
  SIM sim;
  FUNC func;
  SEXP funcR;
   
  sim=(SIM) cqlsR_ExternalPtrAddr(CADR(args),C_ebsim_reactivate);
  PROTECT(funcR=coerceVector(CADDR(args),ENVSXP));
  func=(FUNC) cqlsR_ExternalPtrAddr(C_getExternalPtrForCqlsObj(funcR),C_ebfunc_reactivate);
  sim->func=func;
  UNPROTECT(1);
  return funcR;
}

 
 SEXP R_ebsim_poly_set(SEXP args) {
   SIM sim;
   PT_POLY poly;
   SEXP polyR;
  
   sim=(SIM) cqlsR_ExternalPtrAddr(CADR(args),C_ebsim_reactivate);
   PROTECT(polyR=coerceVector(CADDR(args),ENVSXP));
   poly=(PT_POLY) R_ExternalPtrAddr(C_getExternalPtrForCqlsObj(polyR));
   sim->poly=poly;
   UNPROTECT(1);
   return polyR;
 }


SEXP R_ebsim_run(SEXP args) {
  SIM sim;
  
  sim=(SIM) cqlsR_ExternalPtrAddr(CADR(args),C_ebsim_reactivate);
  //GetRNGstate();//
  //srand48(time(0));//TODO: le faire en R!!!! -> DONE!
  //Rprintf("sim=%p\n",sim);
  ebsim_run(sim);
  //Rprintf("done\n"); 
  //PutRNGstate();
  return R_NilValue;
}

SEXP R_ebsim_nb_dv(SEXP args) {
  SIM sim;
  
  sim=(SIM) cqlsR_ExternalPtrAddr(CADR(args),C_ebsim_reactivate);
  Rprintf("nbPoints: in=%d (%LFx%LF),out=%d (%LFx%LF)\n",
    ebpoly_nbPoints_inside(sim->poly,sim->poly->vg->sizeIn) /*sim->poly->vg->nb_dv_totalIn*/,
    sim->poly->vg->sizeIn[0],sim->poly->vg->sizeIn[1],
    sim->poly->vg->nb_dv_total,
    sim->poly->vg->size[0],sim->poly->vg->size[1]
  );
  return R_NilValue;
}
