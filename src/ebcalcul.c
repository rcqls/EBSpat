/***********************************************/
/*****                CALCUL               *****/
/***********************************************/

/**************************************************/
/*               INCLUDE INSERT FILES             */
/**************************************************/
#include <stdio.h>
#include <time.h>
#include <math.h>
#include "ebvor.h"
#include "eblist.h"

#define DEBUG_DISTzzz
#define DEBUG_CENTREzzz

/*********************************************/
/*    Intersection (sachant qu'elle existe)  */
/*    de deux droites                        */
/*********************************************/
void ebcalcul_intersection(PT_DV p1,PT_DV p2,PT_VV s1,PT_VV s2,DV inter)
{
DOUBLE mgx,mgy,mix,miy,miz,mjx,mjy;
DOUBLE mi,mj,k;
DOUBLE mx,my,s1x,s1y,s2x,s2y;

    s1x = (*s1)->point[0];
    s1y = (*s1)->point[1];

    s2x = (*s2)->point[0];
    s2y = (*s2)->point[1];

    mx = ((*p1)->point[0] + (*p2)->point[0])/2;
    my = ((*p1)->point[1] + (*p2)->point[1])/2;
                             
    mgx = (*p1)->point[0] - mx;
    mgy = (*p1)->point[1] - my;

    mix = s1x - mx;
    miy = s1y - my;

    mjx = s2x - mx;
    mjy = s2y - my;

    mi = mgx*mix + mgy*miy;
    mj = mgx*mjx + mgy*mjy;

    k = fabs(mi/(mi-mj));

    inter->point[0] = s1x + (s2x - s1x)*k;
    inter->point[1] = s1y + (s2y - s1y)*k;
}   

/*******************************************/
/*     Distance euclidienne au carre       */
/*******************************************/
DOUBLE ebcalcul_distance(DOUBLE x1[2],DOUBLE x2[2]) {
  int i;
  //DOUBLE dist;
    //dist=0.0;
    //for(i=0;i<2;i++) dist+=(x1[i]-x2[i])*(x1[i]-x2[i]);
#ifdef DEBUG_DIST    
    //printf("dist([%LF,%LF],[%LF,%LF])^2=%LF\n",x1[0],x1[1],x2[0],x2[1],dist);
    Rprintf("dist([%LF,%LF],[%LF,%LF])^2=%LF\n",x1[0],x1[1],x2[0],x2[1],(x1[0]-x2[0])*(x1[0]-x2[0])+(x1[1]-x2[1])*(x1[1]-x2[1])); 
#endif
    //return dist;
    return (x1[0]-x2[0])*(x1[0]-x2[0])+(x1[1]-x2[1])*(x1[1]-x2[1]);
}

/********************************************/
/* Calcul du produit scalaire de 2 vecteurs */
/********************************************/
DOUBLE ebcalcul_produit(DOUBLE x1[2],DOUBLE x2[2])
{ 
int i;
DOUBLE pd_sc=0;

    //for(i=0;i<2;i++) pd_sc+=x1[i]*x2[i];
    //return(pd_sc);
    return (x1[0]*x2[0])+(x1[1]*x2[1]);
}

/********************************************************************/
/* On calcul la normale oriente a une face de Delaunay              */
/*            CALCUL NORMALE  (produit vectorriel)                  */
/********************************************************************/
void ebcalcul_normale(PT_DV p0,PT_DV p1,PT_DV p2,ST_DV *normale)
{
  ST_DV vecteur;
    
  (*normale).point[0]= -(*p0)->point[1]+(*p1)->point[1];
  (*normale).point[1]=  (*p0)->point[0]-(*p1)->point[0];
    
  vecteur.point[0]=(*p2)->point[0]-(*p1)->point[0];
  vecteur.point[1]=(*p2)->point[1]-(*p1)->point[1];
    
  if(ebcalcul_produit((*normale).point,vecteur.point)>0)
  {
    (*normale).point[0]= -(*normale).point[0];
    (*normale).point[1]= -(*normale).point[1];
  }
} 

/******************************************************/
/* Calcul du centre et du rayon d'une sphere          */
/******************************************************/
void ebcalcul_centre_triangle(PT_DV p0,PT_DV p1,PT_DV p2,DOUBLE *rayon,ST_VV *centre)
{
  DOUBLE norme0,norme1,norme2;
  DOUBLE a00,a11,a01,a10,v0,v1,x0,x1,det;
#ifdef DEBUG_CENTRE
    Rprintf("Tri=([%LF,%LF],[%LF,%LF],[%LF,%LF])\n",(*p0)->point[0],(*p0)->point[1],(*p1)->point[0],(*p1)->point[1],(*p2)->point[0],(*p2)->point[1]);
#endif
  a00 = (*p1)->point[0]-(*p0)->point[0];
  a01 = (*p1)->point[1]-(*p0)->point[1];
  a10 = (*p2)->point[0]-(*p0)->point[0];
  a11 = (*p2)->point[1]-(*p0)->point[1];

  det=a00*a11-a01*a10;
#ifdef DEBUG_CENTRE
  Rprintf("a00=%LF,a01=%LF,a10=%LF,a11=%LF,\n",a00,a01,a10,a11);
  Rprintf("Det=%LF\n",det);
#endif

  if(det==0.) 
  { 
    //printf("\ngermes: %d %d %d",p1-tab_dv,p2-tab_dv);
    Rprintf("\ndanger: le systeme n'est pas inversible");
    exit(1);
  } 
    
  norme0 = ((*p0)->point[0]*(*p0)->point[0])+((*p0)->point[1]*(*p0)->point[1]);
  norme1 = ((*p1)->point[0]*(*p1)->point[0])+((*p1)->point[1]*(*p1)->point[1]);
  norme2 = ((*p2)->point[0]*(*p2)->point[0])+((*p2)->point[1]*(*p2)->point[1]);

  v0 = (norme1-norme0)/2.0;
  v1 = (norme2-norme0)/2.0;
 
#ifdef DEBUG_CENTRE
  Rprintf("norme0=%LF,norme1=%LF,norme2=%LF,v0=%LF,v1=%LF\n",norme0,norme1,norme2,v0,v1);
#endif

  (*centre).point[0]=(a11*v0 - a01*v1)/det;
  (*centre).point[1]=(a00*v1 - a10*v0)/det;
 
#ifdef DEBUG_CENTRE
  Rprintf("centre=[%LF,%LF]\n",(*centre).point[0],(*centre).point[1]);
#endif
    
  *rayon=ebcalcul_distance((*p1)->point,(*centre).point);
}

DOUBLE ebdv_area(PT_DV pt_dv,PT_POLY pt_poly) {
  int j,j1,j2,nb;
  PT_LIST list_polygone=NULL;
  PT_DV pt_p1,pt_p2;
  PT_VV pt_s1,pt_s2;
  //DOUBLE x1,y1,x2,y2;
  DOUBLE r1,r2,surf;

  surf=0.;
  //ebvor_check_structure(pt_poly->vg);
  ebvor_primitive_polygone(pt_poly->vg,pt_dv,&list_polygone,&nb);
  //ebvor_control_list_vv(pt_poly->vg,list_polygone,"area:");
  //printf("nb_voisins=%d=%d\n",nb,eblist_length(list_polygone));
  pt_s1=(PT_VV)eblist_recup_tete(&list_polygone);
  eblist_ins_queue(&list_polygone,pt_s1);
  while(list_polygone!=NULL) {
    pt_s2=(PT_VV)eblist_recup_tete(&list_polygone);
    //x1=(*pt_s1)->point[0];
    //y1=(*pt_s1)->point[1];
    //x2=(*pt_s2)->point[0];
    //y2=(*pt_s2)->point[1];
    //r1=(x1-x2)*(x1-x2)+(y1-y2)*(y1-y2);
    r1=ebcalcul_distance((*pt_s1)->point,(*pt_s2)->point);
    //printf("pt_s1(%LF,%LF),pt_s2(%LF,%LF)\n",(*pt_s1)->point[0],(*pt_s1)->point[1],(*pt_s2)->point[0],(*pt_s2)->point[1]);
    //printf("r1=%LF\n",r1);
    for(j=0;j<3;j++) {
      if((*pt_s1)->nvvs[j]==pt_s2) break;
    }
    ebutil_indices_arete(j,&j1,&j2);
    pt_p1=(*pt_s1)->ndvs[j1];
    pt_p2=(*pt_s1)->ndvs[j2];
    //x1=(*pt_s1)->point[0];
    //y1=(*pt_s1)->point[1];
    //x2=(*pt_s2)->point[0];
    //y2=(*pt_s2)->point[1];
    //r2=(x1-x2)*(x1-x2)+(y1-y2)*(y1-y2);
    if(pt_p1==NULL || pt_p2==NULL) {
      eblist_vider(&list_polygone);
      return infini;
    }
    //printf("pt_p1(%LF,%LF),pt_p2(%LF,%LF)\n",(*pt_p1)->point[0],(*pt_p1)->point[1],(*pt_p2)->point[0],(*pt_p2)->point[1]);
    r2=ebcalcul_distance((*pt_p1)->point,(*pt_p2)->point);
    //printf("r2=%LF\n",r2);
    surf+=sqrt(r1*r2)*.25;
    //printf("surf=%LF\n",surf);
    pt_s1=pt_s2;
  }
  //Rprintf("surf=%LF\n",surf);
  return(surf);
}

/**************************************************/
/*              surface d'un triangle             */
/**************************************************/
DOUBLE ebvv_area(PT_VV triangle) {
  PT_DV pt0=(*triangle)->ndvs[0],pt1=(*triangle)->ndvs[1],pt2=(*triangle)->ndvs[2];
  DOUBLE a,b,c,p;

  a=sqrt(ebcalcul_distance((*pt0)->point,(*pt1)->point));
  b=sqrt(ebcalcul_distance((*pt0)->point,(*pt2)->point));
  c=sqrt(ebcalcul_distance((*pt1)->point,(*pt2)->point));
  p=(a+b+c)*.5;
  //Rprintf("(a,b,c,p)=(%LF,%LF,%LF,%LF)\n",a,b,c,p);
  return sqrt(p*(p-a)*(p-b)*(p-c));
}

/**************************************************/
/*              périmètre d'un triangle             */
/**************************************************/
DOUBLE ebvv_perimeter(PT_VV triangle) {
  PT_DV pt0=(*triangle)->ndvs[0],pt1=(*triangle)->ndvs[1],pt2=(*triangle)->ndvs[2];
  DOUBLE p=0.0;

  p += sqrt(ebcalcul_distance((*pt0)->point,(*pt1)->point));
  p += sqrt(ebcalcul_distance((*pt0)->point,(*pt2)->point));
  p += sqrt(ebcalcul_distance((*pt1)->point,(*pt2)->point));
  return p;
}

/**************************************************/
/*    angles, petit et grand angles d'un triangle */
/**************************************************/
void ebvv_angle(PT_VV pt_som,DOUBLE angle[3]) {
  DOUBLE R12,R13,R23;
  PT_DV pt_p1,pt_p2,pt_p3;

  pt_p1=(*pt_som)->ndvs[0];
  pt_p2=(*pt_som)->ndvs[1];
  pt_p3=(*pt_som)->ndvs[2];
   
  R12 = ebcalcul_distance((*pt_p1)->point,(*pt_p2)->point);
  R13 = ebcalcul_distance( (*pt_p1)->point,(*pt_p3)->point);
  R23 = ebcalcul_distance( (*pt_p2)->point,(*pt_p3)->point);
  angle[0]=(DOUBLE)(acos((R13+R12-R23)/(2*sqrt(R13*R12))));
//printf("R12=%LF,a0=%LF\n",R12,angle[0]);
  angle[1]=(DOUBLE)(acos((R12+R23-R13)/(2*sqrt(R12*R23))));
  angle[2]=(DOUBLE)(M_PI-angle[0]-angle[1]);
  //TODO to order!!
}

DOUBLE ebvv_petitAngle(PT_VV pt_triangle) {
  DOUBLE angle[3],petit;
  
  ebvv_angle(pt_triangle,angle);
  petit=angle[0];
//printf("pa=%LF\n",petit);
  if(angle[1]<petit) petit=angle[1];
  if(angle[2]<petit) petit=angle[2];
  return petit;
}

DOUBLE ebvv_grandAngle(PT_VV pt_triangle) {
  DOUBLE angle[3],grand;
  
  ebvv_angle(pt_triangle,angle);
  grand=angle[0];
  if(angle[1]>grand) grand=angle[1];
  if(angle[2]>grand) grand=angle[2];
  return grand;
}

//TODO: a gadget for time to loop 

//printf("Elapsed time: %u secs.\n", clock()/CLOCKS_PER_SEC);

