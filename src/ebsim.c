#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ebvor.h"
//#include <ebcalcul.h>
#include "ebfunc.h"
#include "eblist.h"

#define DEBUG_SIM_GENz
#define DEBUG_SIM_STRUCTUREz
#define DEBUG_SIMzz
#define AFF_SIMzz
#define CONTROL_SIMz


/****************/
/*    Simule        */
/****************/
void ebsim_free(SIM self) {
if (self!=(SIM)NULL) {
  Free(self);
  }
}

//affect the simulation domain; some square centered
void ebsim_domaineIn_set(SIM self, int domaineIn) {
  self->center[0]=(/**/domaineIn ? self->poly->vg->centerIn[0] :/**/ self->poly->vg->center[0]);
  self->center[1]=(/**/domaineIn ? self->poly->vg->centerIn[1] :/**/ self->poly->vg->center[1]);
  self->size[0]=(domaineIn ? self->poly->vg->sizeIn[0] : self->poly->vg->size[0]);
  self->size[1]=(domaineIn ? self->poly->vg->sizeIn[1] : self->poly->vg->size[1]);
  self->area=self->size[0]*self->size[1];
#ifdef PRINT_EBSIM_DOMAIN
  Rprintf("center=(%LF,%LF),size=(%LF,%LF),area=%LF\n",self->center[0],self->center[1],self->size[0],self->size[1],self->area);
#endif
  //recalculé à chaque fois sans run 
  self->nb_dv_totalExt=self->poly->vg->nb_dv_total - (domaineIn ? /*self->poly->vg->nb_dv_totalIn -> NOT SURE IT IS CORRECT*/ ebpoly_nbPoints_inside(self->poly,self->poly->vg->sizeIn) : self->poly->vg->nb_dv_total);
#ifdef PRINT_EBSIM_DOMAIN
  Rprintf("domaineIn=%d,nb_dv_totalExt=%d\n",domaineIn,self->nb_dv_totalExt);
#endif
}

SIM ebsim_new(FUNC func, PT_POLY poly, int m) {
  SIM self;
  self=(SIM)Calloc(1,ST_SIM);
  self->m=m;
  self->poly=poly;
  self->func=func;
  //self->genMarks=NULL; //=>replaced by poly->vg->marksGen
  //make param form func!!
  //ebfunc_update_funcParam(func,1);
  //kind od func???
  //switch(self->func->kind) {
    //case FUNC_NORMAL: 
  self->local_energy=ebfunc_localEnergy; //break;
    //case FUNC_EXPO: self->local_energy=ebfuncExpo_localEnergy;break;
    //case FUNC_MIXED: self->local_energy=ebfuncMixed_localEnergy;
  //}
  //self->param=ebfunc_param_export(func->modpar);
  //func->param=self->param;
  ebsim_domaineIn_set(self,0);
  return self;
}


void ebsim_gen_dv(SIM self, PT_DV pt_dv,DOUBLE *size, DOUBLE* center) {
  DATA_USER marks=self->poly->vg->marks;
  GetRNGstate();
//printf("center=%LF,%LF\n",center[0],center[1]);
  (*pt_dv)->point[0]=(DOUBLE)(unif_rand()*size[0] - size[0]/2.0 + center[0]);
  (*pt_dv)->point[1]=(DOUBLE)(unif_rand()*size[1] - size[1]/2.0 + center[1]);
  PutRNGstate();
#ifdef DEBUG_SIM_GEN
  Rprintf("Gen:point[0]=%LF,point[1]=%LF\n",(*pt_dv)->point[0],(*pt_dv)->point[1]);
#endif
  if(marks->nbData) {//there is marks!
    DC_curObj(marks->dc,*pt_dv);
    DU_cqlsSEXP_getAllVars_List(cqlsRExpr_eval(self->poly->vg->marksGen),marks);
  }
}


void ebsim_run_one(SIM self) {
  DOUBLE g,g1,p,x,y,V_x_phi;
  int n,nb_dv_total;
  PT_DV 	pt_dv;
  PT_LIST    pt;
  PT_VOR vor;
  PT_POLY poly;
  FUNC func;
  DOUBLE single;
      
#ifdef AFF_SIM
  static int cptIns=0;
  static int okIns=0;
  static int cptSup=0;
  static int okSup=0;
#endif

  single=ebfunc_get_single();//Rprintf("single=%LF\n",single);
  func=self->func;
  poly=self->poly;
  vor=poly->vg;
  nb_dv_total=vor->nb_dv_total-self->nb_dv_totalExt;
  //Rprintf("nb_dv_total(sim)=%d=(%d-%d)\n",nb_dv_total,vor->nb_dv_total,self->nb_dv_totalExt);
  //nécessairement de l'insertion!!!!
  if(nb_dv_total==0) {
#ifdef AFF_SIM
    cptIns++;
#endif
    pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(vor);
    ebsim_gen_dv(self,pt_dv,self->size,self->center);
    ebpoly_make_ins(poly,pt_dv);
    GetRNGstate();
    g=unif_rand();
    PutRNGstate();
#ifdef DEBUG_SIM   
    Rprintf("Insertion0 : T_Utile=(%LF,%LF),point[0]=%LF,point[1]=%LF\n",self->size[0],self->size[1],(*pt_dv)->point[0],(*pt_dv)->point[1]);
#ifdef CONTROL_SIM
    ebvor_control_dv(poly->vg,pt_dv,"Insertion0->");
#endif
    Rprintf("Insertion0 : g=%LF < %LF\n",g,self->area*EXP(single) );
#endif 
    if(g < self->area*EXP(single)) {//QUASI INUTILE PUISQUE CELA DOIT TOUJOURS ETRE VRAI!!! -> ne plus le spécifier dans ebsim_func!!!
#ifdef DEBUG_SIM  
      Rprintf("Insertion0 TO DO\n");
#endif 
      ebpoly_apply_ins(poly);
#ifdef AFF_SIM
      okIns++;
#endif 
    }
    ebpoly_final_ins(poly);
#ifdef DEBUG_SIM
      Rprintf("Insertion0 DONE\n");
#endif   
  } else {
    // suppression d'un point de la configuration
    //      le choix du point est aleatoire      
    GetRNGstate();
    g1=unif_rand();
    PutRNGstate();
#ifdef DEBUG_SIM  
    Rprintf("g1=%LF -> %s\n",g1,(g1<.5 ? "Sup" : "Ins"));
#endif 
    if(g1 < .5) {
#ifdef AFF_SIM
      cptSup++;
#endif
      GetRNGstate();
      do {
        n=(int)(unif_rand()*(vor->nb_dv_total));
#ifdef DEBUG_SIM
        Rprintf("n=%d,nb_dv_total=%d,self->nb_dv_total=%d\n",n,nb_dv_total,vor->nb_dv_total);
#endif
        pt_dv=ebvor_dv_at(vor,n);
#ifdef DEBUG_SIM
        Rprintf("idv=%d,nb_dv=%d\n",pt_dv-vor->tab_dv,vor->nb_dv);
#endif
        //simulation dans tout le domaine ou point dans le domaine intérieur --> ok! 
      } while((self->size[0] < vor->size[0]) && (self->size[1] < vor->size[1]) && !ebdv_dans_domaineIn(pt_dv,vor));
      ebpoly_make_sup(poly,pt_dv);
      g=unif_rand();
      PutRNGstate();
#ifdef CONTROL_SIM
        ebvor_control_dv(poly->vg,pt_dv,"Suppression->");
        V_x_phi=(self->local_energy)(func,poly);
        Rprintf("EXP(0.0)=%LF,V_x_phi=%LF,EXP(V_x_phi)=%LF\n",EXP(-0.0),V_x_phi,EXP(V_x_phi));
	printf("Suppression : g=%LF < %LF\n",g,(DOUBLE)nb_dv_total/self->area*EXP((self->local_energy)(func,poly)) );
#endif
#ifdef DEBUG_SIM_STRUCTURE
printf("After MAKE SUP\n");ebvor_control_structure(vor);ebvor_check_structure(vor);printf("FIN MAKE SUP\n");
#endif
        if(g < (DOUBLE)nb_dv_total/self->area*EXP((self->local_energy)(func,poly))) {
#ifdef DEBUG_SIM
      Rprintf("Suppression TO DO\n");
#endif  
          ebpoly_apply_sup(poly);
#ifdef DEBUG_SIM_STRUCTURE
printf("After APPLY SUP\n");ebvor_control_structure(vor);ebvor_check_structure(vor);printf("FIN APPLY SUP\n");
#endif  
#ifdef AFF_SIM
          okSup++;
#endif
        }
        ebpoly_final_sup(poly);
#ifdef DEBUG_SIM
        Rprintf("Suppression DONE\n");
#endif  
#ifdef DEBUG_SIM_STRUCTURE
printf("After FINAL SUP\n");ebvor_control_structure(vor);ebvor_check_structure(vor);printf("FIN FINAL SUP\n");
#endif	
    } else {
#ifdef AFF_SIM
      cptIns++;
#endif    
    // insertion d'un point dans configuration 
    // le point est choisi uniformement dans S 
      pt_dv=(PT_DV)ebvor_recup_dv_dans_tab(vor);
      ebsim_gen_dv(self,pt_dv,self->size,self->center);
      ebpoly_make_ins(poly,pt_dv);
      //printf("Sortie: ebpoly_make_ins\n");
      GetRNGstate();
      g=unif_rand();
      PutRNGstate();
#ifdef DEBUG_SIM
#ifdef CONTROL_SIM
      ebvor_control_dv(poly->vg,pt_dv,"Insertion->");
#endif
      Rprintf("T_Utile=(%LF,%LF),nb_dv_total=%d,point[0]=%LF,point[1]=%LF\n",self->size[0],self->size[1],nb_dv_total,(*pt_dv)->point[0],(*pt_dv)->point[1]);
      Rprintf("enloc=%LF\n",(self->local_energy)(func,poly));
      Rprintf("Insertion : g=%LF < %LF\n",g,self->area/(DOUBLE)nb_dv_total*EXP(-(self->local_energy)(func,poly)) );
#endif
      if(g < self->area/(DOUBLE)nb_dv_total*EXP(-(self->local_energy)(func,poly))) {
#ifdef DEBUG_SIM
    Rprintf("Insertion TO DO\n");
#endif 
        ebpoly_apply_ins(poly);
#ifdef AFF_SIM
        okIns++;
#endif
      }
      ebpoly_final_ins(poly);
#ifdef DEBUG_SIM
        Rprintf("Insertion DONE\n");
#endif 
    }
  }
#ifdef AFF_SIM
    Rprintf("Ins: %d/%d, Sup: %d/%d\n",okIns,cptIns,okSup,cptSup);
#endif 
}


void ebsim_run(SIM self) {
  int i;
 
  //Rprintf("ebsim func=%p\n",self->func);
  //Rprintf("m=%d\n",self->m); 
  for(i=0;i<self->m;i++) {
#ifdef AFF_SIM  
    Rprintf("i=%d\n",i);
#endif 
    ebsim_run_one(self);
  }
}
