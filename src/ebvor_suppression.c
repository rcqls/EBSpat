/*******************************************/
/*****             SUP                 *****/
/*******************************************/


/***********************************/
/*      INCLUDE INSERT FILES       */
/***********************************/
#include <stdio.h>
//#include <malloc.h>
#include <math.h>
#include "ebvor.h"
#include "ebcalcul.h"
#include "eblist.h"

#define DEBUGSUP_CIRCLEzzz
#define DEBUGSUP_TRIzzz

#define DEBUGSUP_UPDATEzzz
#define DEBUGSUP_APPLYzzz
#define DEBUGSUP_CANCELzzz
#define DEBUGSUP_FINALzzz
/**************************************************/
/*                VARIABLES GLOBALES              */
/**************************************************/
PT_LIST list_aretes_a_traiter=NULL; 
PT_LIST list_dvs_supprimes=NULL;
PT_LIST list_dvs=NULL;
PT_LIST list_arete=NULL; 

/*******************************************/
/*                STRUCTURE                */
/*******************************************/
#define DEBUG_AFF


/***********************************/
/*        TRIANGLE EXISTANT        */
/***********************************/
int ebpoly_arete_existante(PT_POLY self,PT_DV p0,PT_DV p1)
{
    PT_LIST pt,pt_prec,pt_temp;
    int i,j,numero;
    PT_VV pt_vv;

    if(list_arete==NULL) return(-6);                                
    pt_vv=(PT_VV)(list_arete->pt_cur);
    for(i=0;i<3;i++)
      if((*pt_vv)->ndvs[i]==p0) break;

    for(j=0;j<3;j++)
      if((*pt_vv)->ndvs[j]==p1) break;

    if((i<3) && (j<3))
    {
	numero=pt_vv-self->vg->tab_vv;
	pt_temp = list_arete;
	list_arete = list_arete->pt_next;
	Free(pt_temp); 
	return(numero);
    }

    for(pt_prec=list_arete,pt=list_arete->pt_next;pt!=NULL;pt=pt->pt_next,pt_prec=pt_prec->pt_next)
    {
	pt_vv=(PT_VV)(pt->pt_cur);
	for(i=0;i<3;i++) if((*pt_vv)->ndvs[i]==p0) break;
	if(i==3) continue;

	for(j=0;j<3;j++) if((*pt_vv)->ndvs[j]==p1) break;
	
	if(j<3)
	{
	    numero=pt_vv-self->vg->tab_vv;
	    pt_prec->pt_next = pt->pt_next; 
	    Free(pt); 
	    return(numero);
	}
    }
    return(-6); 
}



/************************************************************************/
/* on regarde si la tetraedrisation de delaunay est bien a sphere vide  */
/*            SPHERE VIDE                                               */
/************************************************************************/
int ebpoly_cercle_vide(PT_POLY self,PT_DV pt_p_d,PT_DV p0,PT_DV p1,VV centre,DOUBLE rayon)
{
    PT_LIST pt;
    PT_DV pt_dv_courant;
    DOUBLE ray1;
    int i;

    for(pt=list_dvs;pt!=NULL;pt=pt->pt_next)
    {
	pt_dv_courant = (PT_DV)(pt->pt_cur);
#ifdef DEBUGSUP_CIRCLE
	ebvor_control_dv(self->vg,pt_dv_courant,"cercle vide ");
#endif 
	if((pt_dv_courant==pt_p_d) || (pt_dv_courant==p0) 
	    || (pt_dv_courant==p1)) continue;
    
	ray1=ebcalcul_distance((*pt_dv_courant)->point,centre->point);
#ifdef DEBUGSUP_CIRCLE
	printf("ray1=%LF < rayon=%LF\n",ray1,rayon);
#endif 
	if (ray1<rayon) 
	{
	    return(0);
	}
	if (ray1 == rayon)
	{
	  //printf("\np0=%d %f %f",p0-tab_dv,(p0->point)[0],(p0->point)[1]);
	  //printf("\np1=%d %f %f",p1-tab_dv,(p1->point)[0],(p1->point)[1]);
	  // Rprintf("\npt_dv_courant=%d %f %f",
	  // pt_dv_courant-tab_dv,(pt_dv_courant->point)[0],(pt_dv_courant->point)[1]);
	    Rprintf("\ndanger,le germe est sur le cercle");
	    exit(1);
	}
    }
    return(1);
}

/********************************************************************/
/* on cherche un nouveau tetraedre qui s'appuie sur pt_arete        */
/*                     CHERCHE_TETRAEDRE                            */
/********************************************************************/
char ebpoly_cherche_triangle(PT_POLY self,PT_VV pt_vv_sup,PT_DV p0,PT_DV p1,PT_VV s0,short opp)
{
    PT_LIST pt;
    ST_DV normale, vect;
    ST_VV centre;
    PT_VV pt_vv_nouveau,pt_vv;
    DOUBLE rayon;
    PT_DV pt_dv_dernier,p_p,p_i;
    int i,j,num,k;
    PT_FACE pt_f;
    PT_FACE_EXT pt_fe;

    PT_DV p2=(*s0)->ndvs[opp];
    
    if(pt_vv_sup) ebcalcul_normale(p0,p1,p2,&normale);
    //ebvor_control_list_dv(self->vg, list_dvs_supprimes);
    for(pt=list_dvs_supprimes;pt!=NULL;pt=pt->pt_next) {
	     pt_dv_dernier=(PT_DV)(pt->pt_cur);
#ifdef DEBUGSUP_TRI
      ebvor_control_dv(self->vg,pt_dv_dernier,"Cherche tri : ");
#endif    
	    if((pt_dv_dernier == p0) || (pt_dv_dernier == p1) 
	/*|| (pt_dv_dernier == p2)*/) continue;
	    if(pt_vv_sup) {
	      vect.point[0] = (*pt_dv_dernier)->point[0] - (*p0)->point[0];
	      vect.point[1] = (*pt_dv_dernier)->point[1] - (*p0)->point[1];
	      if(ebcalcul_produit(vect.point,normale.point)<0) continue;
	    }
	    ebcalcul_centre_triangle(pt_dv_dernier,p0,p1,&rayon,&centre);
	    if(ebpoly_cercle_vide(self,pt_dv_dernier,p0,p1,&centre,rayon)) break;
    }

    if (pt==NULL) {
	    Rprintf("\nPas de dernier germe!\n");exit(1);
    } 
    
    eblist_supprime(&list_dvs_supprimes, pt_dv_dernier);
    (*pt_dv_dernier)->boolean=0;
    pt_vv_nouveau=(PT_VV)ebvor_recup_vv_dans_tab(self->vg);
    (*pt_vv_nouveau)->point[0]=centre.point[0];
    (*pt_vv_nouveau)->point[1]=centre.point[1];
    eblist_ins_tete(&(self->list_vvs_nouveaux),pt_vv_nouveau);
    //if(flag==8) (*pt_vv_nouveau)->label=1; /* sommet nouveau */
    //if(flag==2) (*pt_vv_nouveau)->label=1; /* sommet nouveau */
    //TODO: LE TRI EST A FAIRE ICI!!!!
    (*pt_vv_nouveau)->ndvs[0]=p0;
    (*pt_vv_nouveau)->ndvs[1]=p1;
    (*pt_vv_nouveau)->ndvs[2]=pt_dv_dernier;
    (*pt_vv_nouveau)->nvvs[2]=s0;
    //cherche la coté de la face!!! Rmq: ce coté a déjà été déterminé avant!!! (pour trouver p2)
    if(pt_vv_sup) {
	 //Nouvelle face ext: c'est ces faces qu'il faudra ou pas connecter pour mettre à jour le graphe!!!
         pt_fe = (PT_FACE_EXT)Calloc(1,FACE_EXT);
         pt_fe->p_s=s0;
         pt_fe->opp=opp;
         pt_fe->p_sva=pt_vv_sup;
         pt_fe->p_svn=pt_vv_nouveau;
	       if((*(pt_fe->p_s))->label>=0) (*(pt_fe->p_s))->label=-1; //mise de label à -1 pour les sommets ext, (sauf les ext)!!!
         eblist_ins_tete(&(self->list_faces_contour),pt_fe);
	 //printf("Face ext1=(%d,%d)\n",pt_fe->p_s-self->vg->tab_vv,pt_fe->opp);
    } else {
      //ce n'est pas une face extérieure -> mise à jour du voisinage à l'intérieur de s0
      (*s0)->nvvs[opp]=pt_vv_nouveau;
    }
    
    //Strange BUG on MacOSX since it goes there whenever i unitialized:
    //if(i==3) {printf("\nDANGER1!!!");exit(1);}

    ///2 autres aretes du triangle pt_vv_nouveau!!! 
    for(j=0;j<2;j++)
    {
	if(j==0) { 
	    p_p=p0;p_i=p1;k=1;
	} else { 
	    p_p=p1;p_i=p0;k=0;
	}
	num=ebpoly_arete_existante(self,p_p,pt_dv_dernier);//num du sommet ext si arete de l'enveloppe du polygone etoilé!!!
#ifdef DEBUGSUP_TRI
    Rprintf("CONTROL1->(%d)\n",num); ebvor_control_structure(self->vg);
#endif	
      if(num != -6) {//ok c'est une face ext!!!
	    for(i=0;i<3;i++) {
	      if((self->vg->tab_vv[num]->ndvs)[i]==self->point_courant)///n
	 	{
		  pt_vv=(self->vg->tab_vv[num]->nvvs)[i];///n
		  (*pt_vv_nouveau)->nvvs[k]=pt_vv;
		  break;
	        }
	    } 
#ifdef DEBUGSUP_TRI
        Rprintf("CONTROL2->\n"); ebvor_control_structure(self->vg);
#endif
	    if(i==3) {printf("\nDanger 3 !!!");exit(1);}

	    for(i=0;i<3;i++) {if((*pt_vv)->nvvs[i]==(self->vg->tab_vv+num)) {
    		//pt_vv->nvvs[i]=pt_vv_nouveau;   	    	
        pt_fe = (PT_FACE_EXT)Calloc(1,FACE_EXT);
        pt_fe->p_s=pt_vv;
        //printf("Face ext=%d\n",pt_vv_voisin-self->vg->tab_vv);
        pt_fe->opp=i;
	      pt_fe->p_sva=(*pt_vv)->nvvs[i];
        pt_fe->p_svn=pt_vv_nouveau;
	      if((*(pt_fe->p_s))->label>=0) (*(pt_fe->p_s))->label=-1; //mise de label à -1 pour les sommets ext, (sauf les ext)!!!
        eblist_ins_tete(&(self->list_faces_contour),pt_fe);
	      //printf("Face ext2=(%d,%d)\n",pt_fe->p_s-self->vg->tab_vv,pt_fe->opp);
    	  break;
	    }}
#ifdef DEBUGSUP_TRI
      Rprintf("CONTROL3->\n"); ebvor_control_structure(self->vg);
#endif
      } else {//c'est une arête intérieure!!!
	    ///o:pt_f = (PT_ARETE)Calloc(1,ARETE);
            pt_f = (PT_FACE)Calloc(1,FACE);///n
	    if(pt_f == NULL)
            {
		printf("ERREUR : PLUS DE PLACE POUR LES ARETES");
		exit(1);
            }
	    pt_f->p_s=pt_vv_nouveau;
	    pt_f->opp=k; 
            eblist_ins_tete(&list_aretes_a_traiter,pt_f);         
        }
    }
    //ebvor_control_list_vv(self->vg,self->list_vvs_nouveaux,"sommets nouveaux-->");
    return(1);
} 


/***************************************/
/*             MISE_A_JOUR                       */
/*************************************/ 
void ebpoly_update_suppression(PT_POLY self)
{
    PT_VV pt_vv_sup,s0;
    PT_FACE pt_arete;
    int i,nb,i1,i2;
    PT_LIST pt;
    PT_DV pt_dv,p0,p1,p2;  
    PT_FACE_EXT pt_fe;
  
    //printf("Debut suppression\n");
    //utiles uniquement pour suppression!!! 
    list_dvs_supprimes=NULL;
    list_dvs=NULL;
    list_aretes_a_traiter=NULL;
    list_arete=NULL; 
    
    self->list_vvs_nouveaux=NULL;
    self->list_vvs_supprimes=NULL;
    
    if(self->vg->nb_dv_total==0) 
    {
	   Rprintf("plus de points !!");   
        return;
    }
    // A revoir!!!!
    if(self->vg->nb_dv_total==1) {
#ifdef DEBUGSUP_UPDATE
      Rprintf("Etape : plus qu'un point!!!\n");
#endif
      //ebvor_initExt(self->vg); //PB!!!
      ebvor_primitive_polygone(self->vg,self->point_courant, &(self->list_vvs_supprimes), &nb);
      //nouveau sommet s0!!!
      s0=(PT_VV)ebvor_recup_vv_dans_tab(self->vg);
      (self->vg->tab_dv[-1])->num_s=s0-self->vg->tab_vv;///n
      (self->vg->tab_dv[-2])->num_s=s0-self->vg->tab_vv;
      (self->vg->tab_dv[-3])->num_s=s0-self->vg->tab_vv;
      (*s0)->point[0]=0.;
      (*s0)->point[1]=0.;
      (*s0)->label=0;
      eblist_ins_tete(&(self->list_vvs_nouveaux),s0);
      //points et sommets voisins!!!
      //TODO: LA CREATION DES PREMIERS NOUVEAUX VERTICES SONT FACILES A TRIER ICI!!!
      for(i= 0;i<3;i++) {  
	      (*s0)->ndvs[i] = &(self->vg->tab_dv[-(i+1)]);
	      (*s0)->nvvs[i] = &(self->vg->tab_vv[-(i+1)]);
        //face ext
        pt_fe = (PT_FACE_EXT)Calloc(1,FACE_EXT);
        pt_fe->p_s=&(self->vg->tab_vv[-(i+1)]);
        pt_fe->opp=i;
	      pt_fe->p_sva=(*(pt_fe->p_s))->nvvs[i];
        pt_fe->p_svn=s0;
	      if((*(pt_fe->p_s))->label>=0) (*(pt_fe->p_s))->label=-1; //mise de label à -1 pour les sommets ext!!!
        eblist_ins_tete(&(self->list_faces_contour),pt_fe);
      }
      //ebvor_control_structure(self->vg);
      //printf("Fin Etape : plus qu'un point!!!\n");
      return;
    }
#ifdef DEBUGSUP_UPDATE
    Rprintf("Etape 1: voisinage du point à supprimer\n");
#endif
    ebvor_poly(self->vg,self->point_courant, &(self->list_vvs_supprimes), &list_dvs_supprimes,1);//maybe, I need to change 0 in 1 to clean both list!
    eblist_dupliquer(self->list_vvs_supprimes,&list_arete);
    eblist_dupliquer(list_dvs_supprimes,&list_dvs);
    /* eblist_dupliquer(self->list_dvs_supprimes,&list_dvs_modifies);*/
#ifdef DEBUGSUP_UPDATE
    ebvor_control_list_vv(self->vg,self->list_vvs_supprimes,"SUP:");
#endif
    //ebvor_control_list_dv(self->vg,list_dvs_supprimes);
#ifdef DEBUGSUP_UPDATE
    Rprintf("Fin Etape 1 -> Etape2: 1er sommet à supprimer\n");
#endif    
    //1er sommet à supprimer!
    for(pt=self->list_vvs_supprimes;pt!=NULL;pt=pt->pt_next) {
	    pt_vv_sup = (PT_VV)pt->pt_cur;
	    /* Je cherche les 2 germes creant le sommet supprime    */
	    /* Je cherche le sommet ayant p0,p1 pour germe createur */
	    /* s0 et pt_vv_sup forment p0,p1                    */
	    for(i=0;i<3;i++) {
        if((*pt_vv_sup)->ndvs[i]==self->point_courant) {
	        s0=(*pt_vv_sup)->nvvs[i];
	        break;
	      }
	    }
	    if(s0>=self->vg->tab_vv) break;
    }
    
    if(pt==NULL) {
      //printf("Pas de suppression %d %d\n",self->point_courant-self->vg->tab_dv,self->vg->nb_dv_total);
      eblist_vider_dv(&list_dvs_supprimes); 
      eblist_vider(&list_arete);
	    return; 
    } 
    
    if(i==0) { 
      p0=(*pt_vv_sup)->ndvs[1];
      p1=(*pt_vv_sup)->ndvs[2];
    } else if(i==1) { 
      p0=(*pt_vv_sup)->ndvs[0];
      p1=(*pt_vv_sup)->ndvs[2];
    } else if(i==2) { 
      p0=(*pt_vv_sup)->ndvs[0];
      p1=(*pt_vv_sup)->ndvs[1];
    }

    eblist_supprime(&list_dvs_supprimes, p0);
    eblist_supprime(&list_dvs_supprimes, p1);
    (*p0)->boolean=0;
    (*p1)->boolean=0;

    for(i=0;i<3;i++) {
      pt_dv=(*s0)->ndvs[i];
      if((pt_dv!=p0) && (pt_dv!=p1)) {
	  //p2=pt_dv;//p2 est le point opposé à point_courant de s0 (avec (p0,p1) arête commune à s0 et pt_vv_sup)
	      break;
    	}
    } //==> on garde i!!!!
    
    //ebvor_control_vv(self->vg,pt_vv_sup,"");
    //ebvor_control_dv(self->vg,p0);ebvor_control_dv(self->vg,p1,"");ebvor_control_dv(self->vg,p2,"");
#ifdef DEBUGSUP_UPDATE
    Rprintf("Fin Etape 2 -> Etape 3 : nouveaux sommets\n");
#endif
    //pt_vv_sup: sommet à supprimer
    //s0 le sommet voisin à l'exterieur
    //p0,p1: l'arête commune et p2 le 3eme point de s0

    ebpoly_cherche_triangle(self,pt_vv_sup,p0,p1,s0,i);
    //printf("CONTROL ebpoly_cherche_triangle (erreur)->\n"); ebvor_control_structure(self->vg);
    while(list_aretes_a_traiter!=NULL) {
      pt_arete = (PT_FACE) eblist_recup_tete(&list_aretes_a_traiter);
      ebutil_indices_arete(pt_arete->opp,&i1,&i2);
      p0=(*(pt_arete->p_s))->ndvs[i1];
      p1=(*(pt_arete->p_s))->ndvs[i2];

      ebpoly_cherche_triangle(self,NULL,p0,p1,pt_arete->p_s,pt_arete->opp); //-1 -> pas de création de face ext!!!
      Free(pt_arete);
    }
    
    //vider les lists!
    
    eblist_vider_dv(&list_dvs); 
    eblist_vider(&list_arete);
    
    eblist_vider_dv(&list_dvs_supprimes); 
    // eblist_vider(&(self->list_vvs_supprimes)); 
    // eblist_vider(&(self->list_vvs_nouveaux)); 

} 


/************************************/
/*  SUPPRESSION D'UN GERME  */
/**********************************/ 
void ebpoly_make_sup(PT_POLY self,PT_DV pt_dv_sup)
{
#ifdef DEBUGSUP_MAKE
    Rprintf("make suppression->\n");
#endif
  self->mode=-1; //suppression
  self->etat=0; //rien n'est encore fait
  self->point_courant=pt_dv_sup;
  
  ebpoly_update_suppression(self);
  //order the vertices of the new triangles!!!
  ebvor_order_vertices(self->vg,self->list_vvs_nouveaux);
#ifdef DEBUGSUP_MAKE
  ebvor_control_list_vv(self->vg,self->list_vvs_supprimes,"OLD:");
  ebvor_control_list_vv(self->vg,self->list_vvs_nouveaux,"NEW:");
  Rprintf("make suppression->DONE\n");
#endif
//eblist_vv_check_label(self->list_vvs_supprimes,"SUP->supprimes");
//eblist_vv_check_label(self->list_vvs_nouveaux,"SUP->nouveaux");
}



//ATTENTION: ne pas vider les éléments selon apply ou cancel!!!!
void ebpoly_final_sup(PT_POLY self) {
  PT_VV pt_vv;
  PT_LIST *pt,*pt2;
  int j,i1,i2;
  PT_FACE_EXT p_fe;

#ifdef DEBUGSUP_FINAL
  Rprintf("final suppression -> etat=%d\n",self->etat);
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
#ifdef DEBUGSUP_FINAL
      Rprintf("final suppression: Récup des sommets->\n");
#endif
  //vider la list pt avec récup des sommets!!!
  while(*pt != NULL)
  {
    pt_vv = (PT_VV) eblist_recup_tete(pt);
    //printf("rendu dispo=%d\n",pt_vv-self->vg->tab_vv);
    ebvor_new_vv_dispo(self->vg,pt_vv);
  }
  #ifdef DEBUGSUP_FINAL
  if(self->vg->list_vvs_dispo==NULL) Rprintf("Deletion: no more vertices\n");
  #endif
    //vider la list pt2 sans modif des sommets
  /*for(*pt2 != NULL;*pt2!=NULL;*pt2=(*pt2)->pt_next)
  {
    pt_vv = (PT_VV)(*pt2)->pt_cur;
  (*pt_vv)->label=0;
    //mise à jour des num_s!!!
  for(j=0;j<3;j++) (*pt_vv)->ndvs[j]->num_s=pt_vv-self->vg->tab_vv;
    //TODO: Est-ce que les éléments sont bien libérés??? 
}*/
#ifdef DEBUGSUP_FINAL
   Rprintf("final suppression: Récup des sommets->DONE\n");
   Rprintf("final suppression:Update des sommets->\n");
#endif
  while(*pt2 != NULL)
  {
    pt_vv = (PT_VV) eblist_recup_tete(pt2);
    (*pt_vv)->label=0;
    //mise à jour des num_s!!!
    //for(j=0;j<3;j++) (*(*pt_vv)->ndvs[j])->num_s=pt_vv-self->vg->tab_vv;
  }
#ifdef DEBUGSUP_FINAL
   Rprintf("final suppression: Update des sommets->DONE\n");
   Rprintf("final suppression: Vider des faces->\n");
#endif
//vider la list des faces ext
  pt=&(self->list_faces_contour);
  while(*pt != NULL) {
    p_fe = (PT_FACE_EXT) eblist_recup_tete(pt);
    if((*(p_fe->p_s))->label!=-2) (*(p_fe->p_s))->label=0;
    Free(p_fe);
  }
  // eblist_vider(&(self->list_faces_contour));
#ifdef DEBUGSUP_FINAL
   Rprintf("final suppression: Vider faces->DONE\n");
#endif
  // A propos du point_courant
  if(self->etat) {
#ifdef DEBUGSUP_FINAL
   Rprintf("final suppression: Point Libéré->\n");
#endif
    //libère le point supprimé
    //self->point_courant->num_s=-3; /* supprime */
    self->vg->nb_dv_total--;
    if(ebdv_dans_domaineIn(self->point_courant,self->vg)) self->vg->nb_dv_totalIn--;
    ebvor_new_dv_dispo(self->vg,self->point_courant);
#ifdef DEBUGSUP_FINAL
   Rprintf("final suppression: Point Libéré->DONE\n");
#endif 
  } //sinon rien à faire
#ifdef DEBUGSUP_FINAL
      Rprintf("final suppression (etat=%d)-> DONE\n",self->etat);
#endif
}



void ebpoly_apply_sup(PT_POLY self) {
  PT_LIST pt;  
  PT_FACE_EXT p_fe;
  PT_VV pt_vv;
  int j;
  
  if(self->etat) return;
  
#ifdef DEBUGSUP_APPLY
      Rprintf("apply suppression->etat=%d\n",self->etat);
#endif
  //mise à jour des num_s (important pour les primitives de détection des voisins)
  for(pt=self->list_vvs_nouveaux;pt!=NULL;pt=pt->pt_next) {
  pt_vv=(PT_VV)pt->pt_cur;
  for(j=0;j<3;j++) (*(*pt_vv)->ndvs[j])->num_s=pt_vv-self->vg->tab_vv;
  }
  //mise à jour des voisinages avec l'extérieur!
  for(pt=self->list_faces_contour;pt!=NULL;pt=pt->pt_next) {
    p_fe=(PT_FACE_EXT)pt->pt_cur;
    (*(p_fe->p_s))->nvvs[p_fe->opp]=p_fe->p_svn;
    //ebvor_control_vv(self->vg,p_fe->p_s,"sommet ext->");
    //ebvor_control_vv(self->vg,p_fe->p_svn,"svn->");
  }
  //ebvor_control_structure(self->vg);
  self->etat=1;
#ifdef DEBUGSUP_APPLY
  Rprintf("apply suppression->DONE\n");
#endif
}
 
  
 void ebpoly_cancel_sup(PT_POLY self) { 
   PT_LIST pt;  
   PT_FACE_EXT p_fe;
   PT_VV pt_vv;
   int j;
   
   if(!self->etat) return;
   
#ifdef DEBUGSUP_CANCEL
   Rprintf("cancel suppression->etat=%d\n",self->etat);
#endif
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
#ifdef DEBUGSUP_CANCEL
  Rprintf("cancel suppression->DONE\n");
#endif
 }

