#include <stdio.h>
#include <math.h>
#include "ebvor.h"



void ebpoly_vider(PT_POLY self,int domaineInt) {
  int i;
  PT_DV pt_dv;
  
  for( i=0;i<self->vg->nb_dv;i++) {
    pt_dv=self->vg->tab_dv+i;
    //DEBUG:Rprintf("i=%d,pt_poly=%p,pt_dv=%p\n",i,self,pt_dv);
    if(ebdv_isDispo(pt_dv)) continue;///n
    if((!domaineInt) || ebdv_dans_domaineIn(pt_dv,self->vg)) {
      ebpoly_make_sup(self,pt_dv);
      ebpoly_apply_sup(self);
      ebpoly_final_sup(self);
    }
  }
}

int ebpoly_nb_dvs(PT_POLY self,int domaineInt) {
  int i,n=0;
  PT_DV pt_dv;
  
  
  for( i=0;i<self->vg->nb_dv;i++) {
    pt_dv=self->vg->tab_dv+i;
    if(ebdv_isDispo(pt_dv)) continue;///n
    if((!domaineInt) || ebdv_dans_domaineIn(pt_dv,self->vg)) {
      n++;
    }
  }
  return n;
}

//size of the cache is the number of point inside the domain!
int ebpoly_nbPoints_inside(PT_POLY self,DOUBLE *domainSize) {
  int i,nb=0;
  PT_DV pt_dv;

  for(i=0;i<self->vg->nb_dv;i++) {
    pt_dv=self->vg->tab_dv+i;
    ///printf("point=%d\n",*pt_dv);
    if(*pt_dv==(DV)NULL) continue;
    if(ebdv_dans_domaine(pt_dv,self->vg->center,domainSize)) nb++;
  }
  //printf("domainsize=%LF, nb Points=%d\n",domainSize,nb);
  return(nb);
}

 
short ebdv_dans_domaine(PT_DV pt_dv, DOUBLE *centre, DOUBLE *size) {
  short ok=0;
  if((centre[0] -size[0]/2.0 < (*pt_dv)->point[0]) && ((*pt_dv)->point[0] < centre[0] + size[0]/2.0) && (centre[1] - size[1]/2.0 < (*pt_dv)->point[1]) && ((*pt_dv)->point[1] < centre[1] + size[1]/2.0))
    ok=1;
  return ok;
}

short ebdv_dans_domaineIn(PT_DV pt_dv, PT_VOR pt_vor) {
  return ebdv_dans_domaine(pt_dv,pt_vor->centerIn,pt_vor->sizeIn);
}

