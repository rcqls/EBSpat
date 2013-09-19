#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ebvor.h"
#include "ebcalcul.h"
#include "eblist.h"


//PLACER DANS ebsim.c -> chaque fonction d'interaction de paire en dispose d'une!!!
/*DOUBLE ebutil_energie_paire_list_vvs(PT_LIST pt_list, PT_STRAUSS interf) {
  PT_LIST pt; 
  PT_VV sommet,voisin; 
  PT_DV point1,point2;
  int i,i1,i2;
 //initialisation 
  DOUBLE enloc=0.0; 
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {\
    sommet=(PT_VV)pt->pt_cur;
    (*sommet)->label=0;
  }
  //parcourrir les arêtes
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
    sommet=(PT_VV)pt->pt_cur;
    //ebvor_control_vv(self->vg,sommet,"sommet->");
    for(i=0;i<3;i++) {
      voisin=(*sommet)->nvvs[i];
      if(voisin==NULL) continue;
      //ebvor_control_vv(self->vg,voisin,"voisin->");
      if((*voisin)->label != 0) continue;
      //points de l'arête dans la direction i!!!
      ebutil_indices_arete(i,&i1,&i2);
      point1=(*sommet)->ndvs[i1];  point2=(*sommet)->ndvs[i2];
      //printf("paire=%LF\n",eb_strauss_eval(interf,(*point1)->point,(*point2)->point));
      enloc += eb_strauss_eval(interf,(*point1)->point,(*point2)->point);
    }
    (*sommet)->label=1;
  } 
  return enloc;
}

DOUBLE ebpoly_energie_locale_paire(PT_POLY self, PT_STRAUSS interf) {
  PT_LIST pt;  
  PT_VV sommet;
  PT_DV point1,point2;
  int i,j;
  DOUBLE enloc=0.0;
   
  enloc += ebutil_energie_paire_list_vvs(self->list_vvs_nouveaux, interf);
  enloc -= ebutil_energie_paire_list_vvs(self->list_vvs_supprimes, interf);
  
  return enloc;
}*/


void ebutil_indices_arete(int i,int *i1, int *i2) {  
  if(i==0) { 
    *i1=1;
    *i2=2;
  } else if(i==1) { 
    *i1=0;
    *i2=2;
  }  else if(i==2) { 
    *i1=0;
    *i2=1;
  }
}

 int ebutil_index_vv_voisin(PT_VV sommet,PT_VV voisin) {
   int i;
     
   for(i=0;i<3;i++) {
     if((*sommet)->nvvs[i]==voisin) return(i);
   }
   return(-1);
 }

//used later when dim bigger than 2!!!
void ebutil_ordered_indexes(void* *x, int *indx, int n) {
  void* v;
  int i, j, h, iv;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i]; iv = indx[i];
	    j = i;
	    while (j >= h && x[j - h]>v)
		 { x[j] = x[j - h]; indx[j] = indx[j-h]; j -= h; }
	    x[j] = v; indx[j] = iv;
	}
}

//order vertices in sommet
void ebvv_order3(PT_VV s) {
  PT_DV ndvs;
  PT_VV nvvs;

#define ebvv_swap(s,i0,i1) \
      ndvs=(s)->ndvs[i0];(s)->ndvs[i0]=(s)->ndvs[i1];(s)->ndvs[i1]=ndvs; \
      nvvs=(s)->nvvs[i0];(s)->nvvs[i0]=(s)->nvvs[i1];(s)->nvvs[i1]=nvvs;

  //printf("Av swap(%d):%d,%d,%d\n",s,(*s)->ndvs[0],(*s)->ndvs[1],(*s)->ndvs[2]);
  if((*s)->ndvs[0] > (*s)->ndvs[1]) {
    ebvv_swap(*s,0,1);
  }
  if((*s)->ndvs[1] > (*s)->ndvs[2]) {
    ebvv_swap(*s,1,2);
  }
  if((*s)->ndvs[0] > (*s)->ndvs[1]) {
    ebvv_swap(*s,0,1);
  }
  //printf("Ap swap(%d):%d,%d,%d\n",s,(*s)->ndvs[0],(*s)->ndvs[1],(*s)->ndvs[2]);
}
