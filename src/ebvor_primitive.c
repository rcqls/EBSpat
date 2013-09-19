#include <stdio.h>
#include <math.h> 
//#include "ebstruct.h"
#include "ebvor.h"
#include "ebcalcul.h"
#include "eblist.h"


#define AFF_DEBUG_DV
#define AFF_DEBUG_VV
#define AFF_CONTROL_DISPOzzz


/* NOT VERY USEFUL SINCE NO INFORMATION ON THE DUAL EDGE IS OBTAINED!
uncomment ebstruct.h to play with it!

void ebvor_del2Edges(PT_VOR vor,PT_LIST *list_edges) {
{
    int k;
    PT_DV pt_dv,pt_voisin;
    PT_LIST list_dvs=NULL;
    EDGE edge;

    for(k=0;k<vor->nb_dv;k++) 
    {
        pt_dv=(vor->tab_dv)+k;
        if((*pt_dv)->num_s==-3) continue;
        //if(dehors(*pt_dv)) continue;
        ebvor_detecte_voisin(vor,pt_dv,&list_dvs);
        while(list_dvs!=NULL)
        {
            pt_voisin=(PT_DV)recup_tete(&list_dvs);
            (*pt_voisin)->boolean=0;
            if(pt_voisin<pt_dv) continue;
            edge=(EDGE)Calloc(1,ST_EDGE);
            edge->point1=pt_dv;edge->point2=pt_voisin;
            eblist_ins_tete(list_edges,edge);
        }
    }
    
}*/

/*******************************************
* Difference entre les 3 procedures suivantes:
-> ebvor_primitive_polygone: 
  recupère (only) la list des "Voronoi vertices" ainsi que son nombre  
-> ebvor_detecte_voisin:
  recupère (only) la list des "Delaunay vertices"
-> ebvor_poly: 
  récupère les deux lists!
C'est aux utilisateurs de gérer la suppression
de la list généralement fait avec un eblist_recup_tete.

*******************************************/


/*******************************************/
/*            POLY                         */
/*******************************************/
void ebvor_poly(PT_VOR self,PT_DV pt_p1,PT_LIST *list_polygone,PT_LIST *list_dvs,int toClean) {//toClean=1 is safer!
  PT_DV pt_pc;
  PT_VV pt_sd,pt_sc,pt_ss,pt_st; 
  int i,j,compteur;  
  PT_LIST pt;
  
  if(ebdv_isDispo(pt_p1)) {///n
      Rprintf("Erreur dans poly : on cherche le polygone et les voisins d'un germe supprime\n");
      return;
    }
  
  pt_sd=(self->tab_vv)+((*pt_p1)->num_s);
  switch (pt_p1-self->tab_dv) {
    case -1 :
      pt_sd = self->tab_vv-2;
      break;
    case -2 :
      pt_sd = self->tab_vv-3;
      break;
    case -3 :
      pt_sd = self->tab_vv-1;
      break;
    }
    eblist_ins_tete(list_polygone,pt_sd);
    pt_sc=pt_sd;
    pt_st=pt_sd;
    do {
	    for(i=0;i<3;i++) { 
	      compteur=0;
	      pt_ss=(*pt_sc)->nvvs[i];
	      if(pt_ss==NULL) continue;
	      if(pt_ss==pt_st) continue;
	      if(pt_ss==pt_sd) continue;
	      for(j=0;j<3;j++) {
		      pt_pc=(*pt_ss)->ndvs[j];
		      if(pt_pc==pt_p1) {
		        compteur=1;
            eblist_ins_tete(list_polygone,pt_ss);
		        break;
		      }
	      }                                 
	      if(compteur) break;
	    }
	    pt_st=pt_sc;
	    pt_sc=pt_ss;
    } while(compteur);
    
    for(pt= *list_polygone;pt!=NULL;pt=pt->pt_next) {
	    pt_sc=(PT_VV)(pt->pt_cur);
	    for(i=0;i<3;i++) {
	      pt_pc=(*pt_sc)->ndvs[i];
	      if(pt_pc==NULL) continue;
	      if((*pt_pc)->boolean==0) {
          eblist_ins_tete(list_dvs,pt_pc);
	        (*pt_pc)->boolean=1;
	      }
	    }
    }
    (*pt_p1)->boolean=0;
    eblist_supprime(list_dvs,pt_p1);
  if(toClean) {
    ebvor_vv_init_label(*list_polygone);
    ebvor_dv_init_boolean(*list_dvs);
  }
}

/*******************************/
/*            VOISINS          */
/*******************************/
void ebvor_detecte_voisin(PT_VOR self,PT_DV pt_p1,PT_LIST *list_dvs)
{
    PT_LIST list_polygone=NULL;
    PT_DV pt_pc,pt_p;
    PT_VV pt_ss,pt_sd,pt_sc,pt_st; 
    int i,j,compteur;  

    if(ebdv_isDispo(pt_p1)) {///n
	    Rprintf("Erreur dans detecte_voisin : germe supprime\n");
	    return;
    }

    pt_sd=(self->tab_vv)+((*pt_p1)->num_s);
    switch (pt_p1-self->tab_dv) {
	  case -1 :
	    pt_sd = self->tab_vv-2;
	    break;
	  case -2 :
	    pt_sd = self->tab_vv-3;
	    break;
	  case -3 :
	    pt_sd = self->tab_vv-1;
	    break;
	  }
    eblist_ins_tete(&list_polygone,pt_sd);
    pt_sc=pt_sd;
    pt_st=pt_sd;
    do {
	    for(i=0;i<3;i++) { 
	      compteur=0;
	      pt_ss=(*pt_sc)->nvvs[i];
	      if(pt_ss==NULL) continue;
	      if(pt_ss==pt_st) continue;
	      if(pt_ss==pt_sd) continue;
	      for(j=0;j<3;j++) {
	        pt_pc=(*pt_ss)->ndvs[j];
	        if(pt_pc==pt_p1) {
	          compteur=1;
            eblist_ins_tete(&list_polygone,pt_ss);
	          break;
	        }
	      }                                 
	      if(compteur) break;
      }
	    pt_st=pt_sc;
	    pt_sc=pt_ss;
    } while(compteur);
  
    while(list_polygone!=NULL) {
      pt_sd=(PT_VV)eblist_recup_tete(&list_polygone);
	    for(i=0;i<3;i++) {
	      pt_p=(*pt_sd)->ndvs[i];
	      if(pt_p==NULL) continue;
	      if((*pt_p)->boolean==0) {
          eblist_ins_tete(list_dvs,pt_p);
	        (*pt_p)->boolean=1;
	      }
      }
    }
    (*pt_p1)->boolean=0;
    eblist_supprime(list_dvs,pt_p1);
}

/****************************************/
/*           POLYGONE  19/01/93         */
/****************************************/
void ebvor_primitive_polygone(PT_VOR self,PT_DV pt_dv,PT_LIST *list_polygone,int *nb)
{
    PT_DV pt_pc;
    PT_VV pt_ss,pt_sd,pt_sc,pt_st; 
    int i,j,compteur;  

    if(ebdv_isDispo(pt_dv)) {///n
	    Rprintf("Erreur dans polygone : on cherche le polygone d'un germe supprime\n");
	    return;
    }
    *nb=0;
    pt_sd = self->tab_vv+((*pt_dv)->num_s);
    switch (pt_dv-self->tab_dv) {
      case -1 :
	      pt_sd = self->tab_vv-2;
	      break;
      case -2 :
	      pt_sd = self->tab_vv-3;
	      break;
      case -3 :
	      pt_sd = self->tab_vv-1;
	      break;
    }
    //ebvor_control_dv(self,pt_dv,"point:");
    //ebvor_control_vv(self,pt_sd,"premier:");
    eblist_ins_tete(list_polygone,pt_sd);
    (*nb)++;
    pt_sc = pt_sd;
    pt_st = pt_sd;
    do {
      for(i=0; i<3; i++) {
        compteur=0;
	      pt_ss = (*pt_sc)->nvvs[i];
	      if(pt_ss == NULL) continue;
	      if(pt_ss ==  pt_st) continue;
	      if(pt_ss ==  pt_sd) continue;
	      for(j=0;j<3;j++) {
          pt_pc=(*pt_ss)->ndvs[j];
	        if(pt_pc==pt_dv) {
            compteur=1;
            eblist_ins_tete(list_polygone,pt_ss);
            (*nb)++;
	          //printf("nb=%d\n",*nb);
	          //ebvor_control_vv(self,pt_ss,"poly:");
	          break;
	        }
	      }                                 
	      if(compteur) break;
      }
      pt_st=pt_sc;
      pt_sc=pt_ss;
    } while(compteur);
}

//The same as ebvor_primitive_polygone without counting and 
void ebvor_nvvs_for_dv(PT_VOR self,PT_DV point,PT_LIST *list_nvv) {
    PT_DV pt_pc;
    PT_VV pt_ss,pt_sd,pt_sc,pt_st; 
    int i,j,compteur;  

    if(ebdv_isDispo(point)) {///n
	    Rprintf("Erreur dans polygone : on cherche le polygone d'un germe supprime\n");
	    return;
    }
     
    pt_sd = self->tab_vv+((*point)->num_s);
    switch (point-self->tab_dv) {
      case -1 :
	      pt_sd = self->tab_vv-2;
	      break;
      case -2 :
	      pt_sd = self->tab_vv-3;
	      break;
      case -3 :
	      pt_sd = self->tab_vv-1;
	      break;
    }
    //ebvor_control_dv(self,pt_dv,"point:");
    //ebvor_control_vv(self,pt_sd,"premier:");
    eblist_ins_tete(list_nvv,pt_sd);
   
    pt_sc = pt_sd;
    pt_st = pt_sd;
    do {
      for(i=0; i<3; i++) {
        compteur=0;
	      pt_ss = (*pt_sc)->nvvs[i];
	      if(pt_ss == NULL) continue;
	      if(pt_ss ==  pt_st) continue;
	      if(pt_ss ==  pt_sd) continue;
	      for(j=0;j<3;j++) {
          pt_pc=(*pt_ss)->ndvs[j];
	        if(pt_pc==point) {
            compteur=1;
            eblist_ins_tete(list_nvv,pt_ss);
	          //printf("nb=%d\n",*nb);
	          //ebvor_control_vv(self,pt_ss,"poly:");
	          break;
	        }
	      }                                 
	      if(compteur) break;
      }
      pt_st=pt_sc;
      pt_sc=pt_ss;
    } while(compteur);
}


//Poly of order greater than 1
void ebvor_poly_for_dv_at_order(PT_VOR self,PT_DV point,int order,PT_LIST *ListeVoisins) {
  int l;
  PT_DV pt_v;
  PT_LIST pt,ListeArchive=NULL,ListeCourante=NULL;
  
  ebvor_detecte_voisin(self,point,&ListeCourante);
  (*point)->boolean=1;
  
  for(l=1;l<order;l++) {
    while(ListeCourante!=NULL) {
      pt_v=(PT_DV)eblist_recup_tete(&ListeCourante);
      eblist_ins_tete(ListeVoisins,pt_v); 
      ebvor_detecte_voisin(self,pt_v,&ListeArchive);
      (*pt_v)->boolean=1; 
    }
    ListeCourante=ListeArchive;
    ListeArchive=NULL;
  }
  while(ListeCourante!=NULL) {
    pt_v=(PT_DV)eblist_recup_tete(&ListeCourante);
    eblist_ins_tete(ListeVoisins,pt_v); 
  }
  for(pt=*ListeVoisins;pt!=NULL;pt=pt->pt_next) {
    pt_v=(PT_DV)pt->pt_cur;
    (*pt_v)->boolean=0;
  }
  (*point)->boolean=0;
}


//NOT USED and NEED TO BE IMPROVED USING ind instead of boolean and label
//The same but with the voronoi vertices!
void ebvor_nvvs_for_dv_at_order(PT_VOR self,PT_DV point,int order,PT_LIST *nvvs) {
  int l;
  PT_DV pt_dv;
  PT_VV pt_vv;
  PT_LIST pt,ListeArchive=NULL,ListeCourante=NULL,listNvvs=NULL,listDvvs=NULL;
  
  *nvvs=NULL;
  //order 1
  ebvor_poly(self,point,nvvs,&ListeCourante,0);(*point)->boolean=1;
  for(pt=*nvvs;pt!=NULL;pt=pt->pt_next) {pt_vv=(PT_VV)pt->pt_cur;(*pt_vv)->label=1;}
  //order greater than 1
  for(l=1;l<order;l++) {
    while(ListeCourante!=NULL) {
      pt_dv=(PT_DV)eblist_recup_tete(&ListeCourante);
      if((*pt_dv)->boolean==0) {
        eblist_ins_tete(&listDvvs,pt_dv);(*pt_dv)->boolean=1;//this dv is done!
        //find 
        ebvor_poly(self,pt_dv,&listNvvs,&ListeArchive,0);
      }
      while(listNvvs!=NULL) {
        pt_vv=(PT_VV)eblist_recup_tete(&listNvvs);
        if((*pt_vv)->label==0) { 
          eblist_ins_tete(nvvs,pt_vv); //append to ListeNNVS
          (*pt_vv)->label=1;//this vv is done
        }
      }  
    }
    ListeCourante=ListeArchive;
    ListeArchive=NULL;
  }
  //clean boolean
  while(ListeCourante!=NULL) {pt_dv=(PT_DV)eblist_recup_tete(&ListeCourante);(*pt_dv)->boolean=0;}
  //clean label 
  for(pt=*nvvs;pt!=NULL;pt=pt->pt_next) {pt_vv=(PT_DV)pt->pt_cur;(*pt_vv)->label=0;}
  //clean point
  (*point)->boolean=0;
}


//Exactly the same but from an existing vvList (useable on ebfunc since no point is existing in deletion mode)
void ebvor_nvvs_for_vvList_until_order(PT_VOR self,PT_LIST vvList,int order, PT_LIST *nvvs) {
  int l;
  PT_DV pt_dv;
  PT_VV pt_vv;
  PT_LIST pt,ListeArchive=NULL,ListeCourante=NULL,listNvvs=NULL,listDvvs=NULL;
  

  *nvvs=NULL;
  //order 1
  ebvor_dvList_in_vvList(self,vvList,&ListeCourante,1); //each boolean in ListeCourante stays to 1 (no clean)
  
  //printf("AVANT:\n");ebvor_control_structure(self);

  for(pt=vvList;pt!=NULL;pt=pt->pt_next) {
    pt_vv=(PT_VV)pt->pt_cur;
    eblist_ins_tete(nvvs,pt_vv);//append new nvv
    (*pt_vv)->ind=1; //marked done!
  }
  //ebvor_control_list_dv(self,ListeCourante,"ListeCourante:");
  //ebvor_control_list_vv(self,vvList,"vvList:");
  //order greater than 1
  for(l=1;l<order;l++) {
    while(ListeCourante!=NULL) {
      pt_dv=(PT_DV)eblist_recup_tete(&ListeCourante);
      //ebvor_control_dv(self,pt_dv,"dv:");     
      if((*pt_dv)->ind==0) {
        eblist_ins_tete(&listDvvs,pt_dv);(*pt_dv)->ind=1;//this dv is done!
        //find 
        ebvor_poly(self,pt_dv,&listNvvs,&ListeArchive,1);
      }
      while(listNvvs!=NULL) {
        pt_vv=(PT_VV)eblist_recup_tete(&listNvvs);
        if((*pt_vv)->ind==0) { 
          eblist_ins_tete(nvvs,pt_vv); //append to ListeNNVS
          (*pt_vv)->ind=1;//this vv is done
        }
      }  
    }
    ListeCourante=ListeArchive;
    ListeArchive=NULL;
  }
  //ebvor_control_list_dv(self,listDvvs,"dvvsList:");
  //clean boolean and empty ListeCourante
  while(listDvvs!=NULL) {pt_dv=(PT_DV)eblist_recup_tete(&listDvvs);(*pt_dv)->ind=0;}
  //clean label and stay safe the returned *nvvs
  for(pt=*nvvs;pt!=NULL;pt=pt->pt_next) {pt_vv=(PT_DV)pt->pt_cur;(*pt_vv)->ind=0;}
  //ebvor_control_list_vv(self,*nvvs,"nvvsList:");

  //printf("APRES:\n");ebvor_control_structure(self);
}



//Find the dv list from a vv list.
void ebvor_dvList_in_vvList(PT_VOR self,PT_LIST vvList, PT_LIST *dvList,int toClean) {
  PT_LIST pt;
  PT_VV pt_sc;
  PT_DV pt_pc;
  int i;

  //each field boolean of delaunay vertex have to be initially set to 0! Otherwise, the result is wrong!!! But this is ok since each function using boolean is responsible to do the job at the end!
  for(pt=vvList;pt!=NULL;pt=pt->pt_next) {
    pt_sc=(PT_VV)(pt->pt_cur);
    for(i=0;i<3;i++) {
      pt_pc=(*pt_sc)->ndvs[i];
      if(pt_pc==NULL) continue;
      if(pt_pc-self->tab_dv<0) continue;
      if((*pt_pc)->boolean==0) {
	      eblist_ins_tete(dvList,pt_pc);
	      (*pt_pc)->boolean=1;
      }
    }
  }
  if(toClean) {
    //put the boolean to 0!
    for(pt=*dvList;pt!=NULL;pt=pt->pt_next) {
      pt_pc=(PT_DV)(pt->pt_cur);
      (*pt_pc)->boolean=0;
    }
  }
}

void ebvor_all_dvs_from_dv_at_range(PT_VOR self,PT_DV point,DOUBLE range,PT_LIST *list_dvs) {
  PT_DV pt_p,pt_v;
  PT_LIST pile=NULL,list_dvs_vus=NULL,list_voisins=NULL;
  DOUBLE r2=range*range;

    (*point)->ind=-1;
    eblist_ins_tete(&pile,point);
//printf("\nNew Point: range r^2=%LF\n",r2);
    while(pile!=NULL) {
        pt_p=(PT_DV)eblist_recup_tete(&pile);
        ebvor_detecte_voisin(self,pt_p,&list_voisins);
        while(list_voisins!=NULL) {
            pt_v=(PT_DV)eblist_recup_tete(&list_voisins);(*pt_v)->boolean=0; //TODO: IMPORTANT,this is dangerous since if you forget to do it when using detecte_voisin this fails! =>think of a way to do a function ebvor_get_voisin which do that properly!!!
            //printf("pt_v[%p]->ind=%d\n",pt_v,(*pt_v)->ind);
//if((*pt_v)->ind==1) Rprintf("pt_v[%p]->ind=%d,sqrt(dist)=%LF\n",pt_v,(*pt_v)->ind,(DOUBLE)sqrt(ebcalcul_distance((*pt_v)->point,(*point)->point)));
            if( (pt_v - self->tab_dv>=0) && ((*pt_p)->num_s!=-3) && ((*pt_v)->ind==0) ) {
              (*pt_v)->ind=1;
              eblist_ins_tete(&list_dvs_vus,pt_v);
              if(ebcalcul_distance((*pt_v)->point,(*point)->point) < r2) {
                //printf("OK:pt_v[%p]->ind=%d,sqrt(dist)=%LF\n",pt_v,(*pt_v)->ind,(DOUBLE)sqrt(ebcalcul_distance((*pt_v)->point,(*point)->point)));
                eblist_ins_tete(list_dvs,pt_v);
                eblist_ins_tete(&pile,pt_v);
              }
            }
        }
    }

    //clean properly ind!
    (*point)->ind=0;
    while(list_dvs_vus!=NULL) {pt_v=(PT_DV)eblist_recup_tete(&list_dvs_vus);(*pt_v)->ind=0;}
    
  //pt->ind remis à zéro lors de l'utilisation!
}


/**************************************************/
/*                CONTROL_LIST_VVS           */
/**************************************************/
void ebvor_control_list_vv(PT_VOR self,PT_LIST list,char *debut)
{
    PT_LIST pt;
    Rprintf("\n%scontrol list sommet\n",debut);
    for (pt=list;pt!=NULL;pt=pt->pt_next) 
    {
	//printf("\n%d \n",(PT_VV)(pt->pt_cur)-self->tab_vv);
      ebvor_control_vv(self,(PT_VV)(pt->pt_cur),"control ");
    }
    if(list==NULL)
    {
	printf("la list de sommets est vide\n");
    }
}

void ebvor_show_list_vv(PT_VOR self,PT_LIST list,char *debut)
{
    PT_LIST pt;
    PT_VV vv;

    Rprintf("\n%sshow list sommet:",debut);
    for (pt=list;pt!=NULL;pt=pt->pt_next) {
      vv=(PT_VV)(pt->pt_cur);
	    Rprintf(" %d(%d)",vv-self->tab_vv,(*vv)->label);
    }
    if(list==NULL) Rprintf("la list de sommets est vide\n");
    Rprintf("\n");
}

/**************************************************/
/*                CONTROL_LIST_DV             */
/**************************************************/
void ebvor_control_list_dv(PT_VOR self,PT_LIST list,char *debut)
{
    PT_LIST pt;
    PT_DV pt_p;
    Rprintf("\n%scontrol list point\n",debut);
    for (pt=list;pt!=NULL;pt=pt->pt_next) 
    {
      pt_p=(PT_DV)(pt->pt_cur);
      ebvor_control_dv(self,pt_p,"control ");
	//printf("\n%d %d\n",pt_p-self->tab_dv,pt_p->num_s);
    }
    if(list==NULL)
    {
	printf("la list de points est vide\n");
    }
}

/**************************************************/
/*                CONTROL_STRUCTURE               */
/**************************************************/
void ebvor_control_structure(PT_VOR self) {
  int i,j;
  PT_DV pt_p;
  PT_VV pt_s,ptsp;
  
  for(i=-3;i<self->nb_vv;i++)
    {
      ptsp = (PT_VV)(self->tab_vv+i);
      if(ebvv_isDispo(ptsp)) continue;
      ebvor_control_vv(self,ptsp,"Ctl:");
      
      /*printf("\nsommet=%d, label=%d : ",i,(self->tab_vv[i])->label);
      for(j=0;j<3;j++)
	{
      pt_s = ((*ptsp)->nvvs)[j];
	  if(pt_s == NULL) Rprintf("* ");
	  
	  else Rprintf("%d ",pt_s-self->tab_vv);
	}
      Rprintf("   ");
      for(j=0;j<3;j++)
	{
      pt_p = ((*ptsp)->ndvs)[j];
	  if(pt_p == NULL) Rprintf("* ");
	  
	  else Rprintf("%d ",pt_p-self->tab_dv);
    }*/
    }
      Rprintf("\n");
  for(i=-3;i<self->nb_dv;i++)
    {
      pt_p=(PT_DV)(self->tab_dv+i);
      if(ebdv_isDispo(pt_p)) continue;
      ebvor_control_dv(self,&(self->tab_dv[i]),"Ctl:");
    }
#ifdef AFF_CONTROL_DISPO
    Rprintf("Nb points dispo=%d\n",eblist_length(self->list_dvs_dispo));
    ebvor_control_list_dv(self,self->list_dvs_dispo);
    Rprintf("Nb sommets dispo=%d\n",eblist_length(self->list_vvs_dispo));
    ebvor_control_list_vv(self,self->list_vvs_dispo);
#endif
}

/**************************************************/
/*                CONTROL_VV                           */
/**************************************************/
void ebvor_control_vv(PT_VOR self,PT_VV sommet,char *debut)
{
  int j;
  PT_DV pt_p;
  PT_VV pt_s;

  if(ebvv_isDispo(sommet)) return;
#ifdef AFF_DEBUG_VV  
  Rprintf("%ssommet=%d[%LF,%LF], \tlabel=%d, \tind=%d : ",debut,sommet-self->tab_vv,(*sommet)->point[0],(*sommet)->point[1],(*sommet)->label,(*sommet)->ind);
#ifdef AFF_CONTROL_DISPO
if( eblist_dans(sommet,self->list_vvs_dispo)) {
    Rprintf(" -> dispo\n");
    return;
  }
#endif
    
    for(j=0;j<3;j++)
    {
      pt_s = (*sommet)->nvvs[j];
      if(pt_s == NULL) Rprintf("* ");
	  
      else Rprintf("%d ",pt_s-self->tab_vv);
    }
    Rprintf("  ndel : ");
    for(j=0;j<3;j++)
    {
      pt_p = (*sommet)->ndvs[j];
      if(pt_p == NULL) Rprintf("* ");
	  
      else Rprintf("%d ",pt_p-self->tab_dv);
    }
  Rprintf(" FIN\n");
#endif  
}

/**************************************************/
/*                CONTROL_DV                           */
 /**************************************************/
 void ebvor_control_dv(PT_VOR self,PT_DV point,char *debut)
{
  int j;

  if(ebdv_isDispo(point)) return;
#ifdef AFF_DEBUG_DV  
  Rprintf("%spoint=%d: ",debut,point-self->tab_dv);
#ifdef AFF_CONTROL_DISPO
  if(eblist_dans(point,self->list_dvs_dispo)) {
    //printf("Nb points dispo=%d\n",length_list(self->list_dvs_dispo));
    Rprintf(" -> dispo\n");
    return;
  }
#endif
  Rprintf("[");
  for(j=0;j<2;j++)
  {
    Rprintf("%LF ",(*point)->point[j]);
  }
  Rprintf("] (num_s=%d,boolean=%d,ind=%d) FIN\n",(*point)->num_s,(*point)->boolean,(*point)->ind);
#endif  
}

/**************************************************/
/*                CHECK_STRUCTURE                 */
/**************************************************/
void ebvor_check_structure(PT_VOR self)
{
  int i,j,k,i1,i2;
  PT_DV pt_p,pt_s1,pt_s2,pt_v1,pt_v2;
  PT_VV pt_s,ptsp;
  
  Rprintf("Integrité? ->\n");
  for(i=-3;i<self->nb_vv;i++)
  {
    ptsp = (PT_VV)(self->tab_vv+i);
    if(ebvv_isDispo(ptsp)) continue;
    //ebvor_control_vv(self,ptsp);
      //printf("\nsommet=%d, label=%d : ",i,(self->tab_vv[i])->label);
    for(j=0;j<3;j++)
    {
      pt_s = (*ptsp)->nvvs[j];
      if(pt_s != NULL) {
        //ebvor_control_vv(self,pt_s);
        if(pt_s-self->tab_vv<i) break;
        //verif de l'arête commune
        ebutil_indices_arete(j,&i1, &i2);
	pt_s1=(*ptsp)->ndvs[i1];pt_s2=(*ptsp)->ndvs[i2];
        k=ebutil_index_vv_voisin(pt_s,ptsp);
        if(k>=0) {
          ebutil_indices_arete(k,&i1, &i2);
	  pt_v1=(*pt_s)->ndvs[i1];pt_v2=(*pt_s)->ndvs[i2];
          if(!((pt_s1==pt_v1 && pt_s2==pt_v2)|| (pt_s1==pt_v2 && pt_s2==pt_v1))) {
            Rprintf("Intégrité : sommet=%d,voisin[%d]=%d et sommet=%d,voisin[%d]=%d\n",ptsp-self->tab_vv,j,pt_s-self->tab_vv,pt_s-self->tab_vv,k,ptsp-self->tab_vv);
            Rprintf("arete sommet -> %d,%d",pt_s1-self->tab_dv,pt_s2-self->tab_dv);
            Rprintf(" et arete voisin -> %d,%d\n",pt_v1-self->tab_dv,pt_v2-self->tab_dv);
          }
        } else {
          Rprintf("Intégrité : sommet=%d,voisin[%d]=%d mais  pas le contraire!\n",ptsp-self->tab_vv,j,pt_s-self->tab_vv);
          ebvor_control_vv(self,ptsp,"Intégrité : ");
          ebvor_control_vv(self,pt_s,"Intégrité :");
        }
      }
    }
  }
  Rprintf("Fin Intégrité\n");
  
}

/* init label of all voronoi vertices */

void ebvor_all_vvs_init_label(PT_VOR self) {
  PT_VV sommet;
  int i;

  for(i=0;i<self->nb_vv;i++) {
    sommet = (PT_VV)(self->tab_vv+i);
    if(ebvv_isDispo(sommet)) continue;
    (*sommet)->label=0;
  }

}

/**************************************************/
/*          CHECK sommet->label==0              */
/**************************************************/
void ebvor_vv_check_label(PT_VOR self,PT_LIST pt_list,char *debut) {
PT_VV sommet;
PT_LIST pt;
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
    sommet=(PT_VV)pt->pt_cur;
    if ((*sommet)->label !=0) {
      Rprintf("%s:PB: sommet[%d]->label=%d !=0 !\n",debut,sommet-self->tab_vv,(*sommet)->label);
    }
  }
}



void ebvor_vv_init_label(PT_LIST pt_list) {
PT_VV sommet;
PT_LIST pt;
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
    sommet=(PT_VV)pt->pt_cur;
    (*sommet)->label=0;
  }
}


void ebvor_dv_check_boolean(PT_VOR self,PT_LIST pt_list,char *debut) {
PT_DV point;
PT_LIST pt;
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
    point=(PT_DV)pt->pt_cur;
    if ((*point)->boolean !=0) {
      Rprintf("%s:PB: point[%d]->boolean=%d !=0 !\n",debut,point-self->tab_dv,(*point)->boolean);
    }
  }
}

void ebvor_dv_init_boolean(PT_LIST pt_list) {
PT_DV point;
PT_LIST pt;
  for(pt =pt_list;pt!=NULL;pt=pt->pt_next) {
    point=(PT_DV)pt->pt_cur;
    (*point)->boolean=0;
  }
}
/**************************************************/
/*                TRI_VERTICES_LIST_VVS           */
/**************************************************/
void ebvor_order_vertices(PT_VOR self,PT_LIST list) {
    PT_LIST pt;

    if(list==NULL) return;
    for (pt=list;pt!=NULL;pt=pt->pt_next) {
      //printf("\nsommet %d swapped\n",(PT_VV)(pt->pt_cur)-self->tab_vv);
      ebvv_order3((PT_VV)(pt->pt_cur));
    }
}

void ebvor_check_ordered_vertices(PT_VOR self) {
  int i,j,k,i1,i2;
  PT_DV pt_p,pt_s1,pt_s2,pt_v1,pt_v2;
  PT_VV pt_s,ptsp;

   Rprintf("Check order? ->\n");
  for(i=-3;i<self->nb_vv;i++)
  {
    ptsp = (PT_VV)(self->tab_vv+i);
    if(ebvv_isDispo(ptsp)) continue;
    //ebvor_control_vv(self,ptsp);
      //printf("\nsommet=%d, label=%d : ",i,(self->tab_vv[i])->label);
   if( ((*ptsp)->ndvs[0] > (*ptsp)->ndvs[1]) || ((*ptsp)->ndvs[1] > (*ptsp)->ndvs[2])) Rprintf("sommet[%d]:ndel(%d,%d,%d)\n",ptsp-self->tab_vv,(*ptsp)->ndvs[0]-self->tab_dv,(*ptsp)->ndvs[1]-self->tab_dv,(*ptsp)->ndvs[2]-self->tab_dv);
  }
  Rprintf("Check order ->Done\n");
}
