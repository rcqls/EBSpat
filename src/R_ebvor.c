#include "ebvor.h"
#include "ebfunc.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/PrtUtil.h>

#include "CqlsRCom.h"

//#ifndef Win32
//#include <R_ext/eventloop.h>
//#endif

//C functions used by interface wrapper functions
void C_ebvor_marks_set(PT_VOR pt_vor, SEXP vor) {
  SEXP len,name,type,gen;
  int i;
  char *key;

  PROTECT(len=coerceVector(C_getVarForExternalPtr(vor,"del.marks.length"),INTSXP));
  PROTECT(name=coerceVector(C_getVarForExternalPtr(vor,"del.marks.name"),STRSXP));
  PROTECT(type=coerceVector(C_getVarForExternalPtr(vor,"del.marks.type"),INTSXP));
  //Rprintf("length=%d\n",length(len));Rprintf("length=%d\n",length(type));Rprintf("length=%d\n",length(name));
  for(i=0;i<length(len);i++) {
    key=strdup(CHAR(STRING_ELT(name,i)));
    //Rprintf("i=%d,size=%d,type=%d,name=%s\n",i,INTEGER(len)[i],INTEGER(type)[i],key);
    //DC_declare(pt_vor->dc,INTEGER(len)[i],INTEGER(type)[i],key);
    DU_declare(pt_vor->marks,INTEGER(len)[i],INTEGER(type)[i],key);
  }
  PROTECT(gen=coerceVector(C_getVarForExternalPtr(vor,"del.marks.gen"),STRSXP));
  if(gen != R_NilValue && STRING_ELT(gen,0)!=R_NilValue) {
    if(pt_vor->marksGen==NULL) {
      pt_vor->marksGen=cqlsRExpr_new();
      cqlsRExpr_setEnv(pt_vor->marksGen,EBFuncEnvName);
    }
#ifdef debugMode
    Rprintf("marksGen=%s\n",CHAR(STRING_ELT(gen,0)));
#endif
    cqlsRExpr_parse(pt_vor->marksGen,CHAR(STRING_ELT(gen,0)));
  }
  UNPROTECT(4);
}


//C functions used by interface wrapper functions
void C_ebpoly_make_ins(PT_POLY pt_poly,SEXP vect,SEXP marks) {
  PT_DV pt_dv;
  DATA_USER duMarks=pt_poly->vg->marks;
  int i,nb;

  //printf("Ins form %d to %d-1\n",skip,nb);
    pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(pt_poly->vg);
    (*pt_dv)->point[0]=(DOUBLE)(REAL(vect)[0]);
    (*pt_dv)->point[1]=(DOUBLE)(REAL(vect)[1]);
    if(marks!=R_NilValue) {
      if(DC_curObj(duMarks->dc,*pt_dv)) DU_cqlsSEXP_getAllVars_MDF(marks,i,duMarks);
    }
    ebpoly_make_ins(pt_poly,pt_dv);
}

void C_ebpoly_ins(PT_POLY pt_poly,SEXP vect,SEXP marks,int skip) {
  PT_DV pt_dv;
  DATA_USER duMarks=pt_poly->vg->marks;
  int i,nb;

  nb=length(vect)/2;
  //printf("Ins form %d to %d-1\n",skip,nb);
  for(i=skip;i<nb;i++) {
    //Rprintf("i=%d\n",i);
    pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(pt_poly->vg);
    (*pt_dv)->point[0]=(DOUBLE)(REAL(vect)[2*i]);
    (*pt_dv)->point[1]=(DOUBLE)(REAL(vect)[2*i+1]);
    //Rprintf("%d=pt(%LF,%LF)\n",i,(*pt_dv)->point[0],(*pt_dv)->point[1]);
    if(marks!=R_NilValue) {
      if(DC_curObj(duMarks->dc,*pt_dv)) DU_cqlsSEXP_getAllVars_MDF(marks,i,duMarks);
    }
    //Rprintf("0,%d\n",pt_poly);
    ebpoly_make_ins(pt_poly,pt_dv);//Rprintf("1\n");
    ebpoly_apply_ins(pt_poly);//Rprintf("2\n");
    ebpoly_final_ins(pt_poly);//Rprintf("3\n");
  }
}
//Class ebvor 

void C_ebvor_free(SEXP self) {
  PT_VOR vor;
  //Rprintf("GC EBVor is in action!\n");
  vor=(PT_VOR)R_ExternalPtrAddr(self);
  if(vor==NULL) return;
  ebvor_free(vor);
  R_ClearExternalPtr(self);
}

SEXP R_ebvor_free(SEXP args) {
  C_ebvor_free(CADR(args));
  return R_NilValue;
}

SEXP R_ebvor_new(SEXP args) {
  PT_VOR pt_vor;
  SEXP nb;//,dom;
  int nb_max,i;
  //DOUBLE size;

  PROTECT(nb=coerceVector(CADR(args),INTSXP));
  //PROTECT(dom=coerceVector(CADDR(args),REALSXP));
  nb_max=INTEGER(nb)[0];
  //size=REAL(dom)[0];
  //Rprintf("nb_max=%d\n",nb_max);
  pt_vor=ebvor_new(nb_max); //,size);
  //Rprintf("nb_max=%d\n",nb_max);
  UNPROTECT(1);
  return R_MakeExternalPtr(pt_vor, R_NilValue, R_NilValue);
}


SEXP R_ebvor_init(SEXP args) {
  PT_VOR pt_vor;
  SEXP nb, vor,tmp;
  int nb_max;

  PROTECT(vor=CADR(args));
  pt_vor=(PT_VOR) R_ExternalPtrAddr(vor); //no need cqlsR_ExternalPtrAddr
  PROTECT(tmp=allocVector(REALSXP,2));
  REAL(tmp)[0]=pt_vor->size[0];REAL(tmp)[1]=pt_vor->size[1];
  C_defVarForExternalPtr(vor,"size",tmp);//setAttrib(vor,install("size"),tmp);
  UNPROTECT(1);
  PROTECT(tmp=allocVector(REALSXP,2));
  REAL(tmp)[0]=pt_vor->sizeIn[0];REAL(tmp)[1]=pt_vor->sizeIn[1];
  C_defVarForExternalPtr(vor,"sizeIn",tmp);//setAttrib(vor,install("sizeIn"),tmp);
  UNPROTECT(1);
  PROTECT(tmp=allocVector(REALSXP,2));
  REAL(tmp)[0]=pt_vor->center[0];REAL(tmp)[1]=pt_vor->center[1];
  C_defVarForExternalPtr(vor,"center",tmp);//setAttrib(vor,install("center"),tmp);
  UNPROTECT(1);
  PROTECT(tmp=allocVector(REALSXP,2));
  REAL(tmp)[0]=pt_vor->centerIn[0];REAL(tmp)[1]=pt_vor->centerIn[1];
  C_defVarForExternalPtr(vor,"centerIn",tmp);//setAttrib(vor,install("centerIn"),tmp);
  UNPROTECT(2);
  //TODO: gérer center et size en entrée!!!
  return R_NilValue;
}

//only use in cqlsR_ExternalPtrAddr
void* C_ebvor_reactivate(SEXP vor) {
  PT_VOR pt_vor;
  PT_POLY pt_poly;
  SEXP tmp,tmp2,poly,pts,len,type,name,value;
  DOUBLE size;

#ifdef debugMode
  Rprintf("Reactivating ebvor ...\n");
#endif
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(vor,"nbmax"),INTSXP));
  //printf("nbmax=%d\n",INTEGER(tmp)[0]);
  //printf("sizemax=%LF\n",(DOUBLE)(REAL(tmp2)[0]));
  pt_vor=ebvor_new(INTEGER(tmp)[0]);
  UNPROTECT(1);
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(vor,"center"),REALSXP));
  pt_vor->center[0]=REAL(tmp)[0];
  pt_vor->center[1]=REAL(tmp)[1];
  UNPROTECT(1);
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(vor,"centerIn"),REALSXP));
  pt_vor->centerIn[0]=REAL(tmp)[0];
  pt_vor->centerIn[1]=REAL(tmp)[1];
  UNPROTECT(1);
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(vor,"size"),REALSXP));
  pt_vor->size[0]=REAL(tmp)[0];pt_vor->size[1]=REAL(tmp)[1];
  UNPROTECT(1);
  PROTECT(tmp=coerceVector(C_getVarForExternalPtr(vor,"sizeIn"),REALSXP));
  pt_vor->sizeIn[0]=REAL(tmp)[0];pt_vor->sizeIn[1]=REAL(tmp)[1];
  UNPROTECT(1);
  //reactivate poly!
  PROTECT(poly=C_getExternalPtrForExternalPtr(vor,"pl"));
  pt_poly=ebpoly_new(pt_vor);
  R_SetExternalPtrAddr(poly, pt_poly);
  UNPROTECT(1);
  //the marks!!!
  PROTECT(value=coerceVector(C_getVarForExternalPtr(vor,"delMarks"),VECSXP));
  if(value!=R_NilValue && length(value)>0) C_ebvor_marks_set(pt_vor,vor); 
  UNPROTECT(1);
  ebvor_initExt(pt_vor);
  ebvor_initDataExt(pt_vor);
  //insert the points
  PROTECT(pts=coerceVector(C_getVarForExternalPtr(vor,"delVertex"),REALSXP));
  C_ebpoly_ins(pt_poly,pts,value,3);
  UNPROTECT(1);
  //change vorVertex to NULL in order to force update
  C_defVarForExternalPtr(vor,"vorVertex",R_NilValue);
#ifdef debugMode
  Rprintf("ebvor reactivated!\n");
#endif
  return pt_vor;
}

SEXP R_ebvor_reactivate(SEXP args) {
  PT_VOR pt_vor;

  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr_withCFinalizer(CADR(args),C_ebvor_reactivate,C_ebvor_free);
  return R_NilValue;
}

SEXP R_ebvor_marks_set(SEXP args) {
  PT_VOR pt_vor;
  SEXP vor,len,type,name,gen;
  /*int i;
  char *key;*/

  PROTECT(vor=CADR(args));
  pt_vor=(PT_VOR) R_ExternalPtrAddr(vor); //no need cqlsR_ExternalPtrAddr
  C_ebvor_marks_set(pt_vor,vor);
  /*PROTECT(len=coerceVector(getAttrib(vor,install("del.marks.length")),INTSXP));
  PROTECT(name=coerceVector(getAttrib(vor,install("del.marks.name")),STRSXP));
  PROTECT(type=coerceVector(getAttrib(vor,install("del.marks.type")),INTSXP));
  PROTECT(gen=coerceVector(getAttrib(vor,install("del.marks.gen")),STRSXP));
  //Rprintf("length=%d\n",length(len));Rprintf("length=%d\n",length(type));Rprintf("length=%d\n",length(name));
  for(i=0;i<length(len);i++) {
    key=strdup(CHAR(STRING_ELT(name,i)));
    //Rprintf("i=%d,size=%d,type=%d,name=%s\n",i,INTEGER(len)[i],INTEGER(type)[i],key);
    //DC_declare(pt_vor->dc,INTEGER(len)[i],INTEGER(type)[i],key);
    DU_declare(pt_vor->marks,INTEGER(len)[i],INTEGER(type)[i],key);
  }
  //cqlsdataCenter_show(pt_vor->dc);
  //Rprintf("size=%d,type=%d,name=%s\n",INTEGER(len)[0],INTEGER(type)[0],CHAR(STRING_ELT(name,0)));
  UNPROTECT(4);*/
  UNPROTECT(1);
  ebvor_initDataExt(pt_vor);
  return R_NilValue;
}

SEXP R_ebvor_initExt(SEXP args) {
  PT_VOR pt_vor;
  SEXP vor; //,size

  //PROTECT(size=coerceVector(CADDR(args),REALSXP));
  vor=CADR(args);
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);
  //Rprintf("SizeMax=%LF\n",(DOUBLE)(REAL(size)[0]));
  ebvor_initExt(pt_vor);
  //UNPROTECT(1);
  return R_NilValue;
}


SEXP R_ebvor_center_set(SEXP args) {
  PT_VOR pt_vor;
  SEXP center,vor;
  int n;

  PROTECT(center=coerceVector(CADDR(args),REALSXP));
  n=length(center);
  if(n!=2) return R_NilValue;
  vor=CADR(args);
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);
  pt_vor->center[0]=REAL(center)[0];
  pt_vor->center[1]=REAL(center)[1];
  //setAttrib(vor,install("center"),center); ==> in R
  UNPROTECT(1);
  return center;
}

SEXP R_ebvor_centerIn_set(SEXP args) {
  PT_VOR pt_vor;
  SEXP centerIn,vor;
  int n;

  PROTECT(centerIn=coerceVector(CADDR(args),REALSXP));
  n=length(centerIn);
  if(n!=2) return R_NilValue;
  vor=CADR(args);
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);
  pt_vor->centerIn[0]=REAL(centerIn)[0];
  pt_vor->centerIn[1]=REAL(centerIn)[1];
  //setAttrib(vor,install("centerIn"),centerIn);
  UNPROTECT(1);
  return centerIn;
}


SEXP R_ebvor_size_set(SEXP args) {
  PT_VOR pt_vor;
  SEXP size,vor;
  
  PROTECT(size=coerceVector(CADDR(args),REALSXP));
  vor=CADR(args);
  pt_vor=(PT_VOR) R_ExternalPtrAddr(vor);cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);
  pt_vor->size[0]=REAL(size)[0];pt_vor->size[1]=REAL(size)[1];
  //Rprintf("Size=(%LF,%LF)\n",pt_vor->size[0],pt_vor->size[1]);
  //setAttrib(vor,install("size"),size);
  UNPROTECT(1);
  return size;
}

SEXP R_ebvor_sizeIn_set(SEXP args) {
  PT_VOR pt_vor;
  SEXP sizeIn,vor;
  
  PROTECT(sizeIn=coerceVector(CADDR(args),REALSXP));
  vor=CADR(args);
  pt_vor=(PT_VOR) R_ExternalPtrAddr(vor);cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);
  pt_vor->sizeIn[0]=REAL(sizeIn)[0];pt_vor->sizeIn[1]=REAL(sizeIn)[1];
  //Rprintf("SizeIn=(%LF,%LF)\n",pt_vor->sizeIn[0],pt_vor->sizeIn[1]);
  //setAttrib(vor,install("sizeIn"),sizeIn);
  UNPROTECT(1);
  return sizeIn;
}

//Class ebpoly
SEXP R_ebpoly_new(SEXP args) {
  PT_VOR pt_vor;
  PT_POLY pt_poly;
  int nb_max;
  
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(CADR(args),C_ebvor_reactivate);
  pt_poly=ebpoly_new(pt_vor);
  return R_MakeExternalPtr(pt_poly, R_NilValue, R_NilValue);
}

//only reactivate self$vor which is responsible of the reactivation of self!!!
void* C_ebpoly_reactivate(SEXP self) {
  PT_VOR pt_vor;
  SEXP vor,poly;

#ifdef debugMode
  Rprintf("Reactivating ebpoly ...\n");
#endif
  vor=C_getExternalPtrForExternalPtr(self,"vor");//attr(self,"envir")$vor$extPtr
  //reactivation of vor and vor$pl
  pt_vor=C_ebvor_reactivate(vor);
  R_SetExternalPtrAddr(vor, pt_vor);
  poly=C_getExternalPtrForExternalPtr(vor,"pl"); //attr(vor,"envir")$pl$extPtr
  //now vor$pl reactivated!
#ifdef debugMode
  Rprintf("ebpoly reactivated!\n");
#endif
  return R_ExternalPtrAddr(poly);
}


SEXP R_ebpoly_reactivate(SEXP args) {
  PT_POLY pt_poly;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  return R_NilValue;
}


SEXP R_ebpoly_empty(SEXP args) {
  PT_POLY pt_poly;
  SEXP dans;
  
  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  PROTECT(dans=coerceVector(CADDR(args),INTSXP));
  //Rprintf("dans=%d\n",INTEGER(dans)[0]);
  ebpoly_vider(pt_poly,INTEGER(dans)[0]);
  UNPROTECT(1);
  return R_NilValue;
}

SEXP R_ebpoly_ins(SEXP args) {
  SEXP vect,marks;
  PT_POLY pt_poly;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
//Rprintf("ici1\n");
  PROTECT(vect=coerceVector(CADDR(args),REALSXP));
  PROTECT(marks=coerceVector(CADDDR(args),VECSXP));
  C_ebpoly_ins(pt_poly,vect,marks,0);
//Rprintf("ici2\n");
  UNPROTECT(2);
  return R_NilValue;
}

SEXP R_ebpoly_make_ins(SEXP args) {
  SEXP vect,marks;
  PT_POLY pt_poly;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  PROTECT(vect=coerceVector(CADDR(args),REALSXP));
  PROTECT(marks=coerceVector(CADDDR(args),VECSXP));
  C_ebpoly_make_ins(pt_poly,vect,marks);
  UNPROTECT(2);
  return R_NilValue;
}

SEXP R_ebpoly_delete_at(SEXP args) {
  PT_POLY pt_poly;
  PT_DV pt_dv;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  pt_dv=ebvor_dv_at(pt_poly->vg,INTEGER(CADDR(args))[0]-1);
  if(pt_dv!=NULL) {
    ebpoly_make_sup(pt_poly,pt_dv);
    ebpoly_apply_sup(pt_poly);
    ebpoly_final_sup(pt_poly);
  }
  //rb_ary_delete(rb_iv_get(rb_iv_get(self,"@vg"),"@pts"),point);
  return R_NilValue;
}

SEXP R_ebpoly_make_sup(SEXP args) {
  PT_POLY pt_poly;
  PT_DV pt_dv;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  pt_dv=ebvor_dv_at(pt_poly->vg,INTEGER(CADDR(args))[0]-1);
  if(pt_dv!=NULL) ebpoly_make_sup(pt_poly,pt_dv);
  return R_NilValue;
}

SEXP R_ebpoly_apply(SEXP args) {
  PT_POLY pt_poly;
  PT_DV pt_dv;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  ebpoly_apply(pt_poly);
  return R_NilValue;
}

SEXP R_ebpoly_cancel(SEXP args) {
  PT_POLY pt_poly;
  PT_DV pt_dv;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  ebpoly_cancel(pt_poly);
  return R_NilValue;
}

SEXP R_ebpoly_final(SEXP args) {
  PT_POLY pt_poly;
  PT_DV pt_dv;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  ebpoly_final(pt_poly);
  return R_NilValue;
}

SEXP R_ebpoly_current_dv(SEXP args) {
  PT_POLY pt_poly;
  SEXP pointR;

  pt_poly=(PT_POLY) cqlsR_ExternalPtrAddr(CADR(args),C_ebpoly_reactivate);
  PROTECT(pointR=allocVector(REALSXP,2));
  REAL(pointR)[0] = (*(pt_poly->point_courant))->point[0];
  REAL(pointR)[1] = (*(pt_poly->point_courant))->point[1];
  UNPROTECT(1);
  return pointR;
}

SEXP R_ebvor_show_vertex(SEXP args)
{
  PT_VOR pt_vor;
  SEXP vor;
  PT_DV pt_dv;
  int k,cpt;
  
  
  vor=CADR(args);
  pt_vor=(PT_VOR) R_ExternalPtrAddr(vor);

  cpt=0;
  //DEBUG:
  Rprintf("pt_vor=%p\n",pt_vor);
  for(k=-3;k<pt_vor->nb_dv;k++)
  {
    pt_dv=pt_vor->tab_dv+k;
    if(ebdv_isDispo(pt_dv)) continue;
    Rprintf("show:pt[%d/%d]=(%LF,%LF)\n",k,pt_vor->nb_dv,(*pt_dv)->point[0],(*pt_dv)->point[1]);
    cpt++;
  }
  Rprintf("cpt=%d\n",cpt);
  return R_NilValue;
}

SEXP R_ebvor_vertex(SEXP args) {
  PT_VOR pt_vor;
  SEXP vor, delVertex, vorVertex,keys,marks,len,name,type,tmp,tmp2;
  PT_DV pt_dv;
  PT_VV pt_vv;
  PT_LIST l=NULL,lkeys=NULL;
  int k,cpt,key,marked;
  
  
  PROTECT(vor=CADR(args));
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);

  cpt=0;
  //DEBUG:
#ifdef debugMode
  Rprintf("pt_vor=%p\n",pt_vor);
#endif
  for(k=-3;k<pt_vor->nb_dv;k++)
  {
    pt_dv=pt_vor->tab_dv+k;
    if(ebdv_isDispo(pt_dv)) continue;
    //printf("Debut:pt[%d/%d]=(%LF,%LF)\n",k,pt_vor->nb_dv,(*pt_dv)->point[0],(*pt_dv)->point[1]);
    eblist_ins_queue(&l,pt_dv);
    eblist_ins_queue(&lkeys,k);
    cpt++;
  }
#ifdef debugMode
  Rprintf("cpt=%d\n",cpt);
#endif
  PROTECT(delVertex=allocMatrix(REALSXP,2,cpt));
  PROTECT(keys=allocVector(INTSXP,cpt));
  marked=C_getVarForExternalPtr(vor,"delMarks") != R_NilValue;
  //Rprintf("marked=%d\n",marked);
  if(marked) { // && length(len)>0) {
    PROTECT(len=coerceVector(C_getVarForExternalPtr(vor,"del.marks.length"),INTSXP));
    //Rprintf("len=%d\n",length(len));
    PROTECT(marks=allocVector(VECSXP,length(len)));
    PROTECT(name=coerceVector(C_getVarForExternalPtr(vor,"del.marks.name"),STRSXP));
    PROTECT(type=coerceVector(C_getVarForExternalPtr(vor,"del.marks.type"),INTSXP));
    for(k=0;k<length(len);k++) {
      switch(INTEGER(type)[k]) {
	    case DATA_INT:
          PROTECT(tmp=allocVector(INTSXP,INTEGER(len)[k]*cpt));
	        break;
	    case DATA_DBL:
	        PROTECT(tmp=allocVector(REALSXP,INTEGER(len)[k]*cpt));
      }
      if(INTEGER(len)[k]>1) {
        PROTECT(tmp2 = allocVector(INTSXP, 2));
        INTEGER(tmp2)[0] = cpt; INTEGER(tmp2)[1] =INTEGER(len)[k];
        setAttrib(tmp, R_DimSymbol, tmp2);
	UNPROTECT(1);
      }
      SET_VECTOR_ELT(marks,k,tmp);
      UNPROTECT(1);
    }
    //Rprintf("len2=%d\n",length(len));
    setAttrib(marks,R_NamesSymbol,name);
    //Rprintf("len3=%d\n",length(len));
    PROTECT(tmp2 = allocVector(STRSXP, 1));
    //Rprintf("len=%d\n",length(len));
    SET_STRING_ELT(tmp2, 0, mkChar("multi.data.frame"));
    //Rprintf("len=%d\n",length(len));
    classgets(marks, tmp2);
    //Rprintf("len=%d\n",length(len));
    UNPROTECT(1);
    //Rprintf("len=%d\n",length(len));
  }
  //Rprintf("lenTOTO=%d\n",length(len));
  //k=0;
  //while(l!=NULL) {
  for(k=0;k<cpt;k++) {
    //Rprintf("k=%d\n",k);
    pt_dv=(PT_DV)eblist_recup_tete(&l);
    //Rprintf("pt[%d]=(%LF,%LF)\n",k,(*pt_dv)->point[0],(*pt_dv)->point[1]);
    REAL(delVertex)[2*k] = (*pt_dv)->point[0];
    REAL(delVertex)[2*k+1] = (*pt_dv)->point[1];
    key=(int)eblist_recup_tete(&lkeys);
    INTEGER(keys)[k]=key;
    //Rprintf("ICI\n");
    if(marked) { //&& length(len)>0) {
      if(DC_curObj(pt_vor->dc,*pt_dv))
	DU_cqlsSEXP_setAllVars_MDF(marks,k,pt_vor->marks);
      else {
	DU_cqlsSEXP_setAllVarsNA_MDF(marks,k,pt_vor->marks);
      }
      //Rprintf("ICI\n");
    }
  }
  C_defVarForExternalPtr(vor,"delVertex",delVertex);
  C_defVarForExternalPtr(vor,"delId",keys);
  if(marked) {  //&& length(len)>0) {
    C_defVarForExternalPtr(vor,"delMarks",marks);
    UNPROTECT(4);
  }
  UNPROTECT(2);
  l=NULL;lkeys=NULL;
  cpt=0;
  for(k=-3;k<pt_vor->nb_vv;k++)
  {
    pt_vv=pt_vor->tab_vv+k;
    if(ebvv_isDispo(pt_vv)) continue;
    eblist_ins_queue(&l,pt_vv);
    eblist_ins_queue(&lkeys,k);
    cpt++;
  }
  PROTECT(vorVertex=allocMatrix(REALSXP,2,cpt));
  PROTECT(keys=allocVector(INTSXP,cpt));
  for(k=0;k<cpt;k++) {
    pt_vv=(PT_VV)eblist_recup_tete(&l);
    REAL(vorVertex)[2*k] = (*pt_vv)->point[0];
    REAL(vorVertex)[2*k+1] = (*pt_vv)->point[1];
    key=(int)eblist_recup_tete(&lkeys);
    INTEGER(keys)[k]=key;
  }
  C_defVarForExternalPtr(vor,"vorVertex",vorVertex);
  C_defVarForExternalPtr(vor,"vorId",keys);
  UNPROTECT(3);
  return R_NilValue;
  //return vor;
}

 
SEXP R_ebvor_vorEdge(SEXP args) {
  PT_VOR pt_vor;
  SEXP vor, vorEdge;
  PT_VV pt_vv;
  int voisin;
  int cpt,k,j,j2;
  PT_LIST l=NULL;
  
  vor=CADR(args);
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);

  cpt=0;
  for(k=0;k<pt_vor->nb_vv;k++) {
    pt_vv=pt_vor->tab_vv+k;
    if(ebvv_isDispo(pt_vv)) continue;
    //printf("k=%d\n",k);
    for(j=0;j<3;j++) {
      //vorEdge
      if((*pt_vv)->nvvs[j]==NULL) continue;
      voisin=((*pt_vv)->nvvs[j]) - (pt_vor->tab_vv);
      if(voisin > k) {
	      eblist_ins_queue(&l,k);
	      eblist_ins_queue(&l,voisin);
	      cpt++;
      }
    }
  }
  PROTECT(vorEdge=allocMatrix(INTSXP,2,cpt));
  for(k=0;k<cpt;k++) {
    j=(int)eblist_recup_tete(&l);
    j2=(int)eblist_recup_tete(&l);
    INTEGER(vorEdge)[2*k] = j;
    INTEGER(vorEdge)[2*k+1] =j2 ;
  }
  C_defVarForExternalPtr(vor,"vorEdge",vorEdge);
  UNPROTECT(1);
  return R_NilValue;
}

SEXP R_ebvor_vorGraph(SEXP args) {
  PT_VOR pt_vor;
  SEXP vor, vorGraph;
  PT_VV pt_vv;
  int voisin;
  int cpt,k,j;
  PT_LIST l=NULL,lkeys=NULL;
  
  vor=CADR(args);
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);

  cpt=0;
  for(k=0;k<pt_vor->nb_vv;k++) {
    pt_vv=pt_vor->tab_vv+k;
    if(ebvv_isDispo(pt_vv)) continue;
    eblist_ins_queue(&l,pt_vv);
    //eblist_ins_queue(&lkeys,k);
	  cpt++;
  }
#ifdef debugMode
  Rprintf("cpt=%d\n",cpt);
#endif
  PROTECT(vorGraph=allocMatrix(INTSXP,6,cpt));
  for(k=0;k<cpt;k++) {
    pt_vv=(PT_VV)eblist_recup_tete(&l);
    //j=(int)eblist_recup_tete(&lkeys);
    INTEGER(vorGraph)[6*k] = ((*pt_vv)->ndvs[0]) - (pt_vor->tab_dv);
    INTEGER(vorGraph)[6*k+1] = ((*pt_vv)->ndvs[1]) - (pt_vor->tab_dv);
    INTEGER(vorGraph)[6*k+2] = ((*pt_vv)->ndvs[2]) - (pt_vor->tab_dv);
    INTEGER(vorGraph)[6*k+3] = ((*pt_vv)->nvvs[0]) - (pt_vor->tab_vv);
    INTEGER(vorGraph)[6*k+4] = ((*pt_vv)->nvvs[1]) - (pt_vor->tab_vv);
    INTEGER(vorGraph)[6*k+5] = ((*pt_vv)->nvvs[2]) - (pt_vor->tab_vv);
  }
  UNPROTECT(1);
  return vorGraph;
}

SEXP R_ebvor_delEdge(SEXP args) {
  int cpt,k,pt1,pt2;
  PT_DV pt_dv,pt_voisin;
  PT_LIST list_dvs=NULL,l=NULL;
  PT_VOR pt_vor;
  SEXP vor, delEdge;
  
  vor=CADR(args);
  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(vor,C_ebvor_reactivate);

  cpt=0;
  for(k=0;k<pt_vor->nb_dv;k++)
  {
    pt_dv=pt_vor->tab_dv+k;
    if(ebdv_isDispo(pt_dv)) continue;
    //if(dehors(pt_dv)) continue;
    ebvor_detecte_voisin(pt_vor,pt_dv,&list_dvs);
    pt1=pt_dv - pt_vor->tab_dv;
    while(list_dvs!=NULL)
    {
      pt_voisin=(PT_DV)eblist_recup_tete(&list_dvs);
      (*pt_voisin)->boolean=0;
      if(pt_voisin<pt_dv) continue;
      pt2=pt_voisin - pt_vor->tab_dv;
      eblist_ins_queue(&l,pt1);
      eblist_ins_queue(&l,pt2);
      cpt++;
    }
  }
  
  PROTECT(delEdge=allocMatrix(INTSXP,2,cpt));
  for(k=0;k<cpt;k++) {
    pt1=(int)eblist_recup_tete(&l);
    pt2=(int)eblist_recup_tete(&l);
    INTEGER(delEdge)[2*k] = pt1;
    INTEGER(delEdge)[2*k+1] =pt2 ;
  }
  C_defVarForExternalPtr(vor,"delEdge",delEdge);
  UNPROTECT(1);
  return R_NilValue;
}


SEXP R_ebvor_polygon(SEXP args)
{ 
  PT_VOR pt_vor;
  PT_VV pt_s,pt_s1;
  PT_LIST list_vvs=NULL;
  int i,ii,nb_s,compteur;
  SEXP poly,poly2;


  pt_vor=(PT_VOR) cqlsR_ExternalPtrAddr(CADR(args),C_ebvor_reactivate);
  compteur=0;
  poly=NULL;
  for(i=0;i<pt_vor->nb_dv;i++)
    {
      if(ebdv_isDispo(pt_vor->tab_dv+i)) continue;
      ebvor_primitive_polygone(pt_vor,pt_vor->tab_dv+i,&list_vvs,&nb_s);
      //printf("nb=%d\n",nb_s);
      PROTECT(poly2=poly);
      PROTECT(poly=allocVector(INTSXP,compteur+nb_s+2));
      if(poly2!=NULL)  copyVector(poly,poly2);
      //Rprintf("compteur=%d\n",compteur);
      pt_s1=(PT_VV)(list_vvs->pt_cur);
      for(ii = compteur; list_vvs!=NULL; ii++) {
      	//Rprintf("ii=%d\n",ii);
	      pt_s=(PT_VV)eblist_recup_tete(&list_vvs);	
	      //Rprintf("ii=%d\n",ii);
      	INTEGER(poly)[ii] = pt_s - (pt_vor->tab_vv);
      } 
      compteur += nb_s;
      INTEGER(poly)[compteur] = pt_s1 - (pt_vor->tab_vv);
      compteur += 1;
      INTEGER(poly)[compteur] = -5;//to convert in NA
      compteur += 1;
      UNPROTECT(2);
    }
  //Rprintf("avant fin\n");
  return poly;
}


