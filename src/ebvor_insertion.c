/*******************************************/
/******            VORONOI            ******/
/*******************************************/

/**************************************************/
/*             INCLUDE INSERT FILES               */
/**************************************************/
#include <stdio.h>
//#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ebvor.h"
#include "ebcalcul.h"
#include "eblist.h"


#define DEBUGINS_UPDATEzzz
#define DEBUGINS_MAKEzzz
#define DEBUGINS_APPLYzzz
#define DEBUGINS_CANCELzzz
#define DEBUGINS_FINALzzz
/***************************************/
/*  Recherche d'un sommet a supprimer  */
/***************************************/
PT_VV ebvor_cherche_vv_supprime(PT_VOR self,PT_DV pt_dv)
{                         
  PT_VV pt_sc=self->tab_vv-1; 
  PT_VV pt_si;
  ST_DV p1,p2;///n
  PT_DV pt_p;
  char stop =0;
  int i,j,k;
  
  while(stop==0) { 
    stop=1;
    for(i=0;i<3;i++) { 
	    pt_si=(*pt_sc)->nvvs[i];
	    if(pt_si==NULL) continue;
	    if(pt_si<self->tab_vv) continue;
	    if(i==0) j=1; else j=0;
	    pt_p=(*pt_sc)->ndvs[j];
	    for(k=0;k<2;k++) p1.point[k]=(*pt_si)->point[k]-(*pt_sc)->point[k];
	    for(k=0;k<2;k++) p2.point[k]=(*pt_dv)->point[k]-(*pt_p)->point[k];
      if(ebcalcul_produit(p1.point,p2.point)>0) {
	      pt_sc=pt_si;
	      stop=0;
	      break;
      }
    }
  } 
  return(pt_sc);
} 

/**************************************************/
/*      CHERCHE LIST VV SUPPRIME             */
/**************************************************/
void ebpoly_cherche_list_vvs_supprime(PT_POLY self,PT_VV sommet_a_supprimer)
{
  int i; 
  DOUBLE d1,d2; 
  PT_VV pt_sc; 
  PT_LIST list_a_traiter=NULL,pt;
  PT_VV pt_s=sommet_a_supprimer;
  
  //printf("1er sommet_a_supprimer=");ebvor_control_vv(self->vg,pt_s);
  eblist_ins_tete(&(self->list_vvs_supprimes),pt_s);
  eblist_ins_tete(&list_a_traiter,pt_s);
  (*pt_s)->label=1;///n

    while(list_a_traiter != NULL) {
      pt_s = (PT_VV)eblist_recup_tete(&list_a_traiter);
	    for(i=0;i<3;i++) {
	      pt_sc = (*pt_s)->nvvs[i];
	    
	      if((*pt_sc)->label==-2) continue;
	      if((*pt_sc)->label==1)  continue;
	      if((*pt_sc)->label==-1) continue;

	      d1=ebcalcul_distance((*(self->point_courant))->point,(*pt_sc)->point);///n
	      d2=ebcalcul_distance((*((*pt_sc)->ndvs[2]))->point,(*pt_sc)->point);///n
	      if(d1<d2) {
          eblist_ins_tete(&(self->list_vvs_supprimes),pt_sc);
          eblist_ins_tete(&list_a_traiter,pt_sc);
          //printf("sommet_a_supprimer=");ebvor_control_vv(self->vg,pt_sc);
	        (*pt_sc)->label=1;///n /* label d'un sommet a supprimer */
        } else (*pt_sc)->label=-1;///n
      }
    }
}


/**************************************************/
/*                  VORONOI                       */
/**************************************************/
void ebpoly_update_insertion(PT_POLY self)
{
    ST_DV inter;
    PT_LIST pt;
    int i,j,k,i1,j1,k1,num;
    int ind0,ind1,ind2;
    PT_LIST list_nouveaux_dvs=NULL;
    PT_LIST list_vvs_nouveaux=NULL;//,list_vvs_supprimes=NULL;
    PT_DV p0,pt_dv; 
    PT_VV pt_vv_voisin;
    PT_VV pt_vv_sup;  
    PT_VV pt_vv_nouveau,pt_vv_courant,pt_vv_supprime;  
    DOUBLE x2,y2,x1,y1;
    PT_FACE_EXT pt_fe;

#ifdef DEBUGINS_UPDATE
    Rprintf("update insertion->\n");
    Rprintf("nb sommets supprimés=%d\n",eblist_length(self->list_vvs_supprimes));
    ebvor_control_list_vv(self->vg,self->list_vvs_supprimes,"Sup:");
#endif
    /* On considere la list des sommets a supprimer */
    for(pt=self->list_vvs_supprimes;pt!=NULL;pt=pt->pt_next) 
    { 
      pt_vv_sup = (PT_VV)(pt->pt_cur);
#ifdef DEBUGINS_UPDATE
      ebvor_control_vv(self->vg,pt_vv_sup,"Sommet sup parcouru->");
#endif
	/* On considere les sommets voisins du sommet a supprimer */
	for(i=0;i<3;i++)
	{
	  pt_vv_voisin = (*pt_vv_sup)->nvvs[i];
	    /* les deux sommets sont a supprimer => pas de creation de point */
#ifdef DEBUGINS_UPDATE
         ebvor_control_vv(self->vg,pt_vv_voisin,"Sommet voisin parcouru->");
#endif
	    if((*pt_vv_voisin)->label >= 1) continue;
 	    /* Si on arrive ici alors un sommet est a garde un autre a supprimer */ 
	    /*  => Creation d'un nouveau sommet */
	    //if((*pt_vv_voisin)->label == -1)
           //(*pt_vv_voisin)->label = 0; //mise à 0 du label (utilisé dans la recherche des sommets à supprimer)
	    if(i==0)
	      p0 = (*pt_vv_sup)->ndvs[2];
	    else
	      p0 = (*pt_vv_sup)->ndvs[0];
	    ebcalcul_intersection(p0,self->point_courant,pt_vv_sup,pt_vv_voisin,&inter);
#ifdef DEBUGINS_UPDATE
	  Rprintf("inter[%LF,%LF]\n",inter.point[0],inter.point[1]);
#endif	
	  pt_vv_nouveau=(PT_VV)ebvor_recup_vv_dans_tab(self->vg);
	  (*pt_vv_nouveau)->point[0]=inter.point[0];
	  (*pt_vv_nouveau)->point[1]=inter.point[1];
	  //ebvor_control_vv(self->vg,pt_vv_nouveau,"nouveau0->");
         //garde la list des sommets nouveaux
	       eblist_ins_tete(&(self->list_vvs_nouveaux),pt_vv_nouveau);
         eblist_ins_tete(&list_vvs_nouveaux,pt_vv_nouveau);
         //Construction des vertices du nouveau sommet
         if(i==0) {
	      (*pt_vv_nouveau)->ndvs[0]=(*pt_vv_sup)->ndvs[1];
	      (*pt_vv_nouveau)->ndvs[1]=(*pt_vv_sup)->ndvs[2];
	 } else if(i==1) {
	      (*pt_vv_nouveau)->ndvs[0]=(*pt_vv_sup)->ndvs[0];
	      (*pt_vv_nouveau)->ndvs[1]=(*pt_vv_sup)->ndvs[2];
	 } else if(i==2) {
	      (*pt_vv_nouveau)->ndvs[0]=(*pt_vv_sup)->ndvs[0];
	      (*pt_vv_nouveau)->ndvs[1]=(*pt_vv_sup)->ndvs[1];
	 }
	 (*pt_vv_nouveau)->ndvs[2]=self->point_courant;
         //son sommet voisin extérieur!!!
	    (*pt_vv_nouveau)->nvvs[2]=pt_vv_voisin;
          //printf("nouveau(%d)->voisin ext[2]=%d\n",pt_vv_nouveau -self->vg->tab_vv,pt_vv_voisin -self->vg->tab_vv);
         for(j=0;j<3;j++) {
	      if((*pt_vv_voisin)->nvvs[j] == pt_vv_sup) {
                //Nouvelle face ext: c'est ces faces qu'il faudra ou pas connecter pour mettre à jour le graphe!!!
                pt_fe = (PT_FACE_EXT)Calloc(1,FACE_EXT);
                pt_fe->p_s=pt_vv_voisin;
                //printf("Face ext=%d\n",pt_vv_voisin-self->vg->tab_vv);
                pt_fe->opp=j;
                pt_fe->p_sva=pt_vv_sup;
                pt_fe->p_svn=pt_vv_nouveau;
                eblist_ins_tete(&(self->list_faces_contour),pt_fe);
                break;
	       }
	  }
	  num=pt_vv_nouveau-self->vg->tab_vv;
	}
    }
#ifdef DEBUGINS_UPDATE
    Rprintf("nb sommets nouveaux=%d\n",eblist_length(self->list_vvs_nouveaux));
    ebvor_control_list_vv(self->vg,self->list_vvs_nouveaux,"");
#endif
    //Génération des voisinages des nouveaux sommets à l'intérieur du polygone étoilé
    while(list_vvs_nouveaux!= NULL)
    {
      pt_vv_nouveau=(PT_VV)eblist_recup_tete(&list_vvs_nouveaux);
      //ebvor_control_vv(self->vg,pt_vv_nouveau,"nouveau->");
	for(i=0;i<2;i++)
	{
	  if((*pt_vv_nouveau)->nvvs[i] != NULL) continue;
	    for(pt=list_vvs_nouveaux;pt!=NULL;pt=pt->pt_next) 
	    { 
		pt_vv_courant = (PT_VV)(pt->pt_cur);
		if(i==0) k1=1; else k1=0;
		for(i1=0;i1<2;i1++)
		{
		  if((*pt_vv_courant)->ndvs[i1]==(*pt_vv_nouveau)->ndvs[k1])
		    break;
		} 
		if(i1==2) continue;
		if(i1==0) j1=1; else j1=0;
		(*pt_vv_nouveau)->nvvs[i]=pt_vv_courant;
		(*pt_vv_courant)->nvvs[j1]=pt_vv_nouveau;
		break;
            }
      	}
#ifdef DEBUGINS_UPDATE
	ebvor_control_vv(self->vg,pt_vv_nouveau,"nouveau->");
#endif
    }
#ifdef DEBUGINS_UPDATE
    ebvor_control_list_vv(self->vg,self->list_vvs_nouveaux,"NEW:");
    Rprintf("update insertion->DONE\n");
#endif
    //NB: self->list_vvs_nouveaux non vidé!!!
}  
 

/**************************************************/
/*             INSERTION NOUVEAU GERME              */
/**************************************************/
void ebpoly_make_ins(PT_POLY self,PT_DV pt_dv_nouveau)
{
  PT_VV pt_vv_supprime;
#ifdef DEBUGINS_MAKE
  Rprintf("make insertion->\n");
#endif
  self->mode=1; //insertion
  self->etat=0;
  self->point_courant=pt_dv_nouveau;
  pt_vv_supprime=ebvor_cherche_vv_supprime(self->vg,self->point_courant);
  ebpoly_cherche_list_vvs_supprime(self,pt_vv_supprime);
  ebpoly_update_insertion(self);
  //order the vertices of the new triangles!!!
  ebvor_order_vertices(self->vg,self->list_vvs_nouveaux);
  //ebpoly_apply_ins(self);
  //ebpoly_final_ins(self);
#ifdef DEBUGINS_MAKE
  ebvor_control_list_vv(self->vg,self->list_vvs_supprimes,"OLD:");
  ebvor_control_list_vv(self->vg,self->list_vvs_nouveaux,"NEW:");
  Rprintf("make insertion->DONE\n");
#endif
//eblist_vv_check_label(self->list_vvs_supprimes,"INS->supprimes");
//eblist_vv_check_label(self->list_vvs_nouveaux,"INS->nouveaux");
}

//ATTENTION: ne pas vider les éléments selon apply ou cancel!!!!
void ebpoly_final_ins(PT_POLY self) {
  PT_VV pt_vv;
  PT_LIST *pt,*pt2;
  int j;
  PT_FACE_EXT p_fe;

#ifdef DEBUGINS_FINAL
  Rprintf("final insertion -> etat=%d\n",self->etat);
  ebvor_control_list_vv(self->vg,self->list_vvs_supprimes,"OLD:");
  ebvor_control_list_vv(self->vg,self->list_vvs_nouveaux,"NEW:");
#endif
  
  if(self->etat) {//vider list_vvs_supprimes
    pt=&(self->list_vvs_supprimes);
    pt2=&(self->list_vvs_nouveaux);
  } else {//vider list_vvs_nouveaux
    pt=&(self->list_vvs_nouveaux);
    pt2=&(self->list_vvs_supprimes);
  }
  //vider la list pt avec récup des sommets!!!
  while(*pt != NULL)
    {
      pt_vv = (PT_VV)eblist_recup_tete(pt);
      //printf("rendu dispo=%d\n",pt_vv-self->vg->tab_vv);
      ebvor_new_vv_dispo(self->vg,pt_vv);
      //printf("*(pt_vv=%d)=%ld\n",pt_vv,*pt_vv);
    }
    //ebvor_control_list_vv(self->vg,self->vg->list_vvs_dispo,"DISPO:");
    //DEBUG: if(self->vg->list_vvs_dispo==NULL) Rprintf("Insertion: no more vertices\n");
    //vider la list pt2 sans modif des sommets
    /*for(*pt2 != NULL;*pt2!=NULL;*pt2=(*pt2)->pt_next)
    {
      pt_vv = (PT_VV)(*pt2)->pt_cur;
    (*pt_vv)->label=0;
    for(j=0;j<3;j++) (*pt_vv)->ndvs[j]->num_s=pt_vv-self->vg->tab_vv;
    //TODO: Est-ce que les éléments sont bien libérés??? 
}*/
    while(*pt2 != NULL)
    {
      pt_vv = (PT_VV)eblist_recup_tete(pt2);
      (*pt_vv)->label=0;
      //for(j=0;j<3;j++) (*(*pt_vv)->ndvs[j])->num_s=pt_vv-self->vg->tab_vv;
    }
     //vider la list des faces ext
    pt=&(self->list_faces_contour);
    while(*pt != NULL)
    {
      p_fe = (PT_FACE_EXT)eblist_recup_tete(pt);
      if((*(p_fe->p_s))->label!=-2) (*(p_fe->p_s))->label=0;
      Free(p_fe);
    }
    //vider(&(self->list_faces_contour)); 
    
    // A propos du point_courant
    if(self->etat) {
      self->vg->nb_dv_total++;
      if(ebdv_dans_domaineIn(self->point_courant,self->vg)) self->vg->nb_dv_totalIn++;
    } else {
      //Recup du point car non inséré!!
      ebvor_new_dv_dispo(self->vg,self->point_courant);
      //ins_tete(&(self->vg->list_dvs_dispo),self->point_courant);
    }
#ifdef DEBUGINS_FINAL
printf("final insertion (etat=%d)-> DONE\n",self->etat);
#endif
}


void ebpoly_apply_ins(PT_POLY self) {
  PT_LIST pt;
  PT_FACE_EXT p_fe;
  PT_VV pt_vv;
  int j;

#ifdef DEBUGINS_APPLY
  Rprintf("apply insertion->etat=%d\n",self->etat);
#endif
  if(self->etat) return;

  //mise à jour des num_s (important pour les primitives de détection des voisins)
  for(pt=self->list_vvs_nouveaux;pt!=NULL;pt=pt->pt_next) {
    pt_vv=(PT_VV)pt->pt_cur;
    for(j=0;j<3;j++) (*(*pt_vv)->ndvs[j])->num_s=pt_vv-self->vg->tab_vv;
  }
  //mise à jour des voisinages avec l'extérieur!
  for(pt=self->list_faces_contour;pt!=NULL;pt=pt->pt_next) {
    p_fe=(PT_FACE_EXT)pt->pt_cur;
    (*(p_fe->p_s))->nvvs[p_fe->opp]=p_fe->p_svn;
  } 
  self->etat=1;
#ifdef DEBUGINS_APPLY
  Rprintf("apply insertion->DONE\n");
#endif
}
 
  
 void ebpoly_cancel_ins(PT_POLY self) { 
   PT_LIST pt;  
   PT_FACE_EXT p_fe;
   PT_VV pt_vv;
   int j;
   
#ifdef DEBUGINS_CANCEL
   Rprintf("cancel insertion->etat=%d\n",self->etat);
#endif
   if(!self->etat) return;
  //mise à jour des num_s (important pour les primitives de détection des voisins)
  for(pt=self->list_vvs_supprimes;pt!=NULL;pt=pt->pt_next) {
    pt_vv=(PT_VV)pt->pt_cur;
    for(j=0;j<3;j++) (*(*pt_vv)->ndvs[j])->num_s=pt_vv-self->vg->tab_vv;
  }
   //mise à jour des voisinages avec l'extérieur!
   for(pt=self->list_faces_contour;pt!=NULL;pt=pt->pt_next)
   {
     p_fe=(PT_FACE_EXT)pt->pt_cur;
     (*(p_fe->p_s))->nvvs[p_fe->opp]=p_fe->p_sva;
   } 
   self->etat=0;
#ifdef DEBUGINS_CANCEL
   Rprintf("cancel insertion->DONE\n");
#endif
 }
