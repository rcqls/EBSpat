//#include <stdio.h>
//#include <malloc.h>
//#include <stdlib.h>
#include <stdio.h>
#include <string.h>
//#include <math.h>
#include "ebvor.h"
#include "ebcalcul.h"

#define DEBUG_VV_DISPOzzz

//OPTIONS!!!
#define LIST_DISPO_ORDO
//projet de ne pas vider les éléments de list_vvs_dispo et list_dvs_dispo!!!!
#define MODE_DISPO_FREE

DV ebdv_new() {///n
  DV self;
  
  self=(DV)Calloc(1,ST_DV);
  self->data=NULL;
  self->boolean=0;
  self->ind=0;
  return self;
}

//ATTENTION: l'argument est *DV et non DV!!! -> conserve le résultat à NULL
void ebdv_free(DV *self) {///n
  if(*self!=(DV)NULL) {
    //free the data field
    Free(*self);
    *self=(DV)NULL;
  }
}

short ebdv_isDispo(PT_DV self) {
#ifdef MODE_DISPO_FREE
  return (*self==(DV)NULL);
#else
  return ((*self)->num_s==-3);
#endif
}

VV ebvv_new() {///n
  VV self;
  
  self=(VV)Calloc(1,ST_VV);
  self->data=NULL;
  self->label=0;
  self->ind=0;
  return self;
}

//ATTENTION: l'argument est *VV et non VV!!! -> conserve le résultat à NULL
void ebvv_free(VV *self) {///n
  if(*self!=(VV)NULL) {
    Free(*self);
    *self=(VV)NULL;
  }
}

short ebvv_isDispo(PT_VV self) {
#ifdef MODE_DISPO_FREE
  return (*self==(VV)NULL);
#else
  return ((*self)->label==-3);
#endif
}
/**************************************************/
/*                  NEW_VORONOI                  */
/**************************************************/
PT_VOR ebvor_new(int nb_dv_max) //,DOUBLE size) 
{
  int i;
  PT_VOR self;
   
  //printf("ici\n");
  self=(PT_VOR)Calloc(1,ST_VOR);
  //printf("ici\n");
  self->nb_dv_max=nb_dv_max;
  //printf("nb_dv_max=%d\n", self->nb_dv_max);
  self->nb_vv_max=3.5*nb_dv_max;
  //printf("nb_vv_max=%d\n", self->nb_vv_max);
  self->tab_dv=(PT_DV)Calloc(self->nb_dv_max+3,DV);Pb_M(self->tab_dv,34);///n:ATTENTION DV  est maintenant un pointeur!!!
  //printf("ici\n");
  self->tab_dv+=3;self->nb_dv=0;
  for(i=0;i<self->nb_dv_max;i++) self->tab_dv[i]=(DV)NULL;
  self->tab_vv=(PT_VV)Calloc(self->nb_vv_max+3,VV);Pb_M(self->tab_vv,34);///n:ATTENTION VV est maintenat un pointeur!!!
//printf("VV declared\n");
  self->tab_vv+=3;self->nb_vv=0;
  for(i=0;i<self->nb_vv_max;i++) self->tab_vv[i]=(VV)NULL;
//printf("VV done\n"); 
  self->list_dispo_ordo=1; //tri des list_dispo
  self->list_vvs_dispo=NULL;
  self->list_dvs_dispo=NULL;
  //R self->list_vvs_supprimes=NULL;

  // self->center[0]=0.0;self->center[1]=0.0;
  // self->size[0]=700.0;self->size[1]=700.0;
  // self->centerIn[0]=self->center[0];self->centerIn[1]=self->center[1];
  // self->sizeIn[0]=500.0;self->sizeIn[1]=500.0;

  // self->center[0]=domain[0];self->center[1]=domain[1];
  // self->size[0]=domain[2];self->size[1]=domain[3];
  // self->centerIn[0]=domain[4];self->centerIn[1]=domain[5];
  // self->sizeIn[0]=domain[6];self->sizeIn[1]=domain[7];

  //Rprintf("size=%LF,%LF,%LF,%LF,%LF,%LF,%LF,%LF\n",self->center[0],self->center[1],self->size[0],self->size[1],self->centerIn[0],self->centerIn[1],self->sizeIn[0],self->sizeIn[1]);

  self->nb_dv_total=0;
  self->nb_dv_totalIn=0;
//printf("struct done\n");
  self->tab_dv[-1]=ebdv_new();///n
  self->tab_dv[-2]=ebdv_new();///n
  self->tab_dv[-3]=ebdv_new();///n
  self->tab_vv[-1]=ebvv_new();///n
  self->tab_vv[-2]=ebvv_new();///n
  self->tab_vv[-3]=ebvv_new();///n
  //ebvor_initExt(self,size); //BUG R!!!
//printf("ici\n");
  //fflush(stdout);
  //printf("ici\n");
  //DataCenter
  self->dc=cqlsdataCenter_new();
  self->marks=cqlsdataUser_new(self->dc);
  self->marksGen=NULL;
  ebfunc_init();//since now 
  //self->kppv=cqlsdataUser_new(self->dc);
  return(self);
}

/**************************************************/
/*               INITIALISATION                   */
/**************************************************/
///n: main change
///n:1) (self->tab_dv[?]).point[?] replace by (self->tab_dv[?])->point[?]
///n:2) pt_s0->point[?] remplacé par (*pt_s0)->point[?]
void ebvor_initExt(PT_VOR self) //,DOUBLE size)
{
    int i,j;
    PT_VV pt_s0;

    DOUBLE r2,c2,h2,l2, extInf,size;


    if(self->size[0] > self->size[1]) size=self->size[0]; else size=self->size[1];
    //Rprintf("size=%LF\n",size);
    //Rprintf("size=%LF,%LF,%LF,%LF,%LF,%LF,%LF,%LF\n",self->center[0],self->center[1],self->size[0],self->size[1],self->centerIn[0],self->centerIn[1],self->sizeIn[0],self->sizeIn[1]);

    size=size*(DOUBLE)1.1;
    r2 = (DOUBLE)sqrt((DOUBLE)2.) * size;
    c2 = (DOUBLE)sqrt((DOUBLE)6.) * size;
    //h2 = racine3 * c2 / 2.0;
    l2 = 2. * (DOUBLE)sqrt((DOUBLE)2.) * size;//h2 * 2.0 /3.0
    //printf("ici2\n");
    
    (self->tab_dv[-1])->point[0] = 0. + self->center[0];
    (self->tab_dv[-1])->point[1] = l2 + self->center[1];
    
    (self->tab_dv[-2])->point[0] = c2 + self->center[0];//racine3 * 5000.;
    (self->tab_dv[-2])->point[1] = -r2 + self->center[1];//-5000.;
    
    (self->tab_dv[-3])->point[0] = -c2 + self->center[0];//-racine3 * 5000.;
    (self->tab_dv[-3])->point[1] = -r2 + self->center[1];//-5000.;
    //Rprintf("ici2\n");

    pt_s0=(PT_VV)ebvor_recup_vv_dans_tab(self); 
    //Rprintf("ici222\n");
    (self->tab_dv[-1])->num_s=pt_s0-self->tab_vv;
    (self->tab_dv[-2])->num_s=pt_s0-self->tab_vv;
    (self->tab_dv[-3])->num_s=pt_s0-self->tab_vv;
    //Rprintf("ici222\n");
    (*pt_s0)->point[0]=(DOUBLE)0. + self->center[0];
    (*pt_s0)->point[1]=(DOUBLE)0. + self->center[1];
    (*pt_s0)->label=0;(*pt_s0)->ind=0;
    //printf("ici2\n");
    
   
    extInf = (DOUBLE)(1000000.*size);
    
    self->tab_vv[-3]->point[0] = extInf*(self->tab_dv[-1]->point[0]-self->center[0]+self->tab_dv[-2]->point[0]-self->center[0])+ self->center[0];
    self->tab_vv[-3]->point[1] = extInf*(self->tab_dv[-1]->point[1]-self->center[1]+self->tab_dv[-2]->point[1]-self->center[1])+ self->center[1];
    
    self->tab_vv[-2]->point[0] = extInf*(self->tab_dv[-1]->point[0]-self->center[0]+self->tab_dv[-3]->point[0]-self->center[0])+ self->center[0];
    self->tab_vv[-2]->point[1] = extInf*(self->tab_dv[-1]->point[1]-self->center[1]+self->tab_dv[-3]->point[1]-self->center[1])+ self->center[1];
     
    self->tab_vv[-1]->point[0] = extInf*(self->tab_dv[-3]->point[0]-self->center[0]+self->tab_dv[-2]->point[0]-self->center[0])+ self->center[0];
    self->tab_vv[-1]->point[1] = extInf*(self->tab_dv[-3]->point[1]-self->center[1]+self->tab_dv[-2]->point[1]-self->center[1])+ self->center[1];
    

    for(i= -3;i<0;i++) {
      self->tab_vv[i]->label = -2;
      for(j=0;j<3;j++) {
        if(i == -(j+1)) {
      	 self->tab_vv[i]->ndvs[j] = NULL;
      	 self->tab_vv[i]->nvvs[j] = pt_s0;
        } else {
      	 self->tab_vv[i]->ndvs[j] = &(self->tab_dv[-(j+1)]);
      	 self->tab_vv[i]->nvvs[j] = NULL;
        }
      }
      for(j= 0;j<3;j++) {  
      	(*pt_s0)->ndvs[j] = &(self->tab_dv[-(j+1)]);
      	(*pt_s0)->nvvs[j] = &(self->tab_vv[-(j+1)]);
      }
    }
    //Rprintf("ici2222\n");
}

void ebvor_initDataExt(PT_VOR self) {
  int i;

  
  for(i= -3;i<0;i++) {
    cqlsdataCenter_newDataForObj(self->dc,self->tab_dv[i]);
  }
}

/**************************************************/
/*             LIBERE DVEUR                    */
/**************************************************/
void ebvor_free(PT_VOR self)
{
  cqlsdataUser_free(self->marks);
  cqlsdataUser_free(self->kppv);
  cqlsdataCenter_free(self->dc);
  eblist_vider(&(self->list_dvs_dispo)); 
//   eblist_vider(&(self->list_vvs_dispo));
  self->tab_dv -= 3;
  Free((self->tab_dv));
  self->tab_vv -= 3;
  Free((self->tab_vv));
  Free(self);
}


/**************************************************/
/*        POLY CLASS : pour insérer et supprimer    */
/**************************************************/
PT_POLY ebpoly_new(PT_VOR vg) {
  PT_POLY self; 

  self=(PT_POLY)Calloc(1,POLY);
  
  self->mode=0;
  self->etat=0;
  self->vg=vg;
  self->point_courant=NULL; //point traité
  
  //list des sommets avant et après insertion ou suppression!!!
  self->list_vvs_supprimes=NULL;
  self->list_vvs_nouveaux=NULL;
  self->list_faces_contour=NULL;
  return(self);
}

void ebpoly_vider_lists(PT_POLY self)
{
  eblist_vider(&(self->list_vvs_supprimes));
  eblist_vider(&(self->list_vvs_nouveaux));
  eblist_vider(&(self->list_faces_contour));
}

void ebpoly_free(PT_POLY self) {
  ebpoly_vider_lists(self);
  Free(self);
}

/**************************************************/
/*           INSERT_DV_DANS_TAB_DV          */
/**************************************************/
PT_VV ebvor_recup_vv_dans_tab(PT_VOR self)
{
  PT_VV pt_vv_recup;

#ifdef MODE_DISPO_FREE
  //printf("list_dispo=%d\n",self->list_vvs_dispo);
    if(self->list_vvs_dispo!=NULL) {           
      pt_vv_recup=(PT_VV)eblist_recup_tete(&(self->list_vvs_dispo));
    } else {
      if(self->nb_vv==self->nb_vv_max) {
        ///n:TODO: si besoin agrandir tab_vv!!!
	printf("\nPLUS DE PLACE POUR LES VVS");
	exit(0);
      }
      self->nb_vv++;
      //printf("nb_vv=%d\n",self->nb_vv);
      pt_vv_recup=&(self->tab_vv[self->nb_vv-1]);///n
    }
    *pt_vv_recup=ebvv_new();///n: un nouveau sommet est dynamiquement alloué
    //printf("sommet recup=%d\n",pt_vv_recup-self->tab_vv);
    //ebvor_control_vv(self,pt_vv_recup,"recup->");
    return pt_vv_recup;
#else
    if(self->list_vvs_dispo!=NULL) {
      pt_vv_recup=(PT_VV)eblist_recup_tete(&(self->list_vvs_dispo));
      (*pt_vv_recup)->label=0;(*pt_vv_recup)->ind=0;///n:car mis à NULL avant dans ebvor_new_vv_dispo
    } else {
      if(self->nb_vv==self->nb_vv_max) {
        ///n:TODO: si besoin agrandir tab_vv!!!
        Rprintf("\nPLUS DE PLACE POUR LES VVS");
        exit(0);
      }
      self->nb_vv++;
      //printf("nb_vv=%d\n",self->nb_vv);
      pt_vv_recup=&(self->tab_vv[self->nb_vv-1]);
      *pt_vv_recup=ebvv_new();/// un nouveau sommet est dynamiquement alloué
   }
    return pt_vv_recup;
#endif
}



/**************************************************/
/*           INSERT_DV_DANS_TAB_DV          */
/**************************************************/
PT_DV ebvor_recup_dv_dans_tab(PT_VOR self)
{
    PT_DV pt_dv_recup;
//#ifdef MODE_DISPO_FREE
    if(self->list_dvs_dispo!=NULL) { 
      pt_dv_recup=(PT_DV)eblist_recup_tete(&(self->list_dvs_dispo));
    } else {
      if(self->nb_dv==self->nb_dv_max) {
	///n:TODO: si besoin agrandir tab_dv!!!
        Rprintf("\nPLUS DE PLACE POUR LES DVS\n");
        exit(0);
      }
      //self->tab_dv[self->nb_dv].ind=0;
      self->nb_dv++;
      pt_dv_recup=self->tab_dv+(self->nb_dv-1);///n
    }
    //ebvor_control_dv(self,&(self->tab_dv[self->nb_dv-1]),"Recup en fin: ");
    *pt_dv_recup=ebdv_new();///n: un nouveau point est dynamiquement alloué
    //if dataCenter is not empty!!!
    if(self->dc->n) {
      cqlsdataCenter_newDataForObj(self->dc,*pt_dv_recup);///n
    }
    (*pt_dv_recup)->num_s= -3;
    return(pt_dv_recup);
//#else
    /*if(self->list_dvs_dispo!=NULL) { 
      pt_dv_recup=(PT_DV)eblist_recup_tete(&(self->list_dvs_dispo));
        //ebvor_control_dv(self,pt_dv_recup,"Recup: ");
      //pt_dv_recup->num_s=0;///n:car mis à NULL avant dans ebvor_new_dv_dispo
    } else {
      if(self->nb_dv==self->nb_dv_max) {
	///n:TODO: si besoin agrandir tab_dv!!!
        Rprintf("\nPLUS DE PLACE POUR LES DVS\n");
        exit(0);
    }
      //self->tab_dv[self->nb_dv].ind=0;
    self->nb_dv++;
    pt_dv_recup=self->tab_dv+(self->nb_dv-1);///n
    *pt_dv_recup=ebdv_new();///n: un nouveau point est dynamiquement alloué
    }
    //ebvor_control_dv(self,&(self->tab_dv[self->nb_dv-1]),"Recup en fin: ");
    (*pt_dv_recup)->num_s= -3;///ATTENTION: pour l'autre cas c'était mis à 0???
    return(pt_dv_recup);*/
//#endif
}

/**************************************************/
/*           NOUVEAU DV DISPO                          */
/**************************************************/
void ebvor_new_vv_dispo(PT_VOR self,PT_VV pt_vv) {
  int j;
  VV som;
  
#ifdef DEBUG_VV_DISPO
printf("new sommet dispo->(&tab_vv=%d)\n",self->tab_vv);
#endif
#ifdef LIST_DISPO_ORDO  
  if(self->list_dispo_ordo) {  
    //check if this is  the last
    //printf("nb_vv=%d\n",self->nb_vv);
    if((self->nb_vv>0) && (pt_vv-self->tab_vv == self->nb_vv-1)) {
      //printf("LAST!!!\n");
      self->nb_vv--;
/*#ifndef MODE_DISPO_FREE
      ebvv_free(pt_vv);
#endif*/
      //delete the last
      while(self->list_vvs_dispo!=NULL && ebvv_isDispo(self->tab_vv +(self->nb_vv-1))) {///n
        eblist_supprime(&(self->list_vvs_dispo),&(self->tab_vv[self->nb_vv-1]));
/*#ifndef MODE_DISPO_FREE
        ebvv_free(&(self->tab_vv[self->nb_vv-1]));
#endif*/
        self->nb_vv--;
      }
    } else {
      //printf("New Sommet : avant=%d ",eblist_length(self->list_vvs_dispo));
      eblist_ins_ordonnee(&(self->list_vvs_dispo),pt_vv);
      //printf("et après=%d \n",eblist_length(self->list_vvs_dispo));
    }
  } else 
#endif  
  {
    eblist_ins_tete(&(self->list_vvs_dispo),pt_vv);
  }
  /*#ifdef MODE_DISPO_FREE*/
  ///n: vider l'élément VV pointé
  ebvv_free(pt_vv);
  /*#endif*/
  //printf("pt_vv(rang=%d)=%d,*pt_vv=%d?=(VV)NULL=%d\n",pt_vv-self->tab_vv,pt_vv,*pt_vv,(VV)NULL);
#ifdef DEBUG_VV_DISPO
      ebvor_control_list_vv(self,self->list_vvs_dispo,"DISPO:");
      Rprintf("new sommet dispo->DONE\n");
#endif
}

/**************************************************/
/*           NOUVEAU DV DISPO                          */
/**************************************************/
void ebvor_new_dv_dispo(PT_VOR self,PT_DV pt_dv) {
#ifdef LIST_DISPO_ORDO  
  if(self->list_dispo_ordo) {   
    //check if this is the last
    if((self->nb_dv>0) && (pt_dv-self->tab_dv == self->nb_dv-1)) {
      //printf("LAST!!!\n");
      self->nb_dv--;
/*#ifndef MODE_DISPO_FREE
      ebdv_free(pt_dv); //puisque dernier!!!
#endif*/
      //delete the last
      while(self->list_dvs_dispo!=NULL && ebdv_isDispo(self->tab_dv + (self->nb_dv-1))) {///n
        eblist_supprime(&(self->list_dvs_dispo),&(self->tab_dv[self->nb_dv-1]));
/*#ifndef MODE_DISPO_FREE
        ebdv_free(&(self->tab_dv[self->nb_dv-1]));
#endif*/
        self->nb_dv--;
      }
    } else { 
      //printf("New Point : %d \n",length_list(self->list_dvs_dispo));
      eblist_ins_ordonnee(&(self->list_dvs_dispo),pt_dv);
    }
  } else 
#endif   
  {
    eblist_ins_tete(&(self->list_dvs_dispo),pt_dv);  
  }
  /*#ifdef MODE_DISPO_FREE*/
  ///n: vider (*pt_dv)->data
  if(self->dc->n) {
    cqlsdataCenter_freeDataForObj(self->dc,*pt_dv);
  }
  ///n: vider l'élément DV pointé
  ebdv_free(pt_dv);
  /*#endif*/
}


//i commence à 0!!!
PT_DV ebvor_dv_at(PT_VOR self,int i) {
  int ii,jj=-1;
  
  if(i >= self->nb_dv_total) return (PT_DV)NULL;
  for(ii=0;ii<self->nb_dv;ii++) {
    ///o:if(self->tab_dv[ii].num_s!=-3) jj++;
    if(!ebdv_isDispo(self->tab_dv+ii)) jj++;///n
    if(jj==i) break;
  } 
  return(self->tab_dv+ii);
} 

PT_VV ebvor_vv_at(PT_VOR self,int i) {
  int ii,jj=-1;
 
  for(ii=0;ii<self->nb_vv;ii++) {
    ///o:if(self->tab_vv[ii].label!=-3) jj++;
    if(ebvv_isDispo(self->tab_vv+ii)) jj++;///n
    if(jj==i) break;
}
  if(jj > i) return (PT_VV)NULL;
  return(self->tab_vv+ii);
} 



/**************************************************/
/*                       VIDER                    */
/**************************************************/
void  eblist_vider_dv(PT_LIST *list)
{
    PT_LIST temps;
    PT_DV pt_p;

    while(*list != NULL) {
	    temps = *list;
	    pt_p=(PT_DV)temps->pt_cur;
	    (*pt_p)->boolean=0;
      (*pt_p)->ind=0;
	    *list = (*list)->pt_next;
	    Free(temps); 
    }
}
