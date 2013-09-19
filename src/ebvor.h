#ifndef _ebvor_h
#define _ebvor_h
#include "cqlsR.h"
#include "eblist.h"
#include <math.h>

#define DOUBLE long double
#define LF Lf
#define EXP expl

//#define infini (DOUBLE)10000000000000.;
#define infini INFINITY
//ID : classe abstraite pour lire le champ id
typedef struct st_id {
  int id;
} *ID;

/* TODO: Maybe replace the name of PT_VV with PT_VV (Pointer on Voronoi Vertex) and PT_DV with PT_DV (Pointer on Delaunay Vertex)*/

/* Structure des germes de Delaunay */ //Aurait mieux fallu l'appeler delvertex plutot que point.
typedef struct st_dv {
  void    **data;///n:external data
  DOUBLE  point[2];
  char 	  boolean;
  int 	  num_s;
  short   ind;
}ST_DV,*DV,**PT_DV;
//ENORME CHANGEMENT: pour rendre dynamique l'allocation des DVs et conserver la structure d'indexation linéaire tab_dv

extern DV ebdv_new(); ///n:pour allouer la mémoire de la structure pour chaque point
extern void ebdv_free(); ///n:pour libérer
extern short ebdv_isDispo();
extern short ebdv_dans_domaine();
extern short ebdv_dans_domaineIn();
extern DOUBLE ebdv_area();//dans ebcalcul.c

/* Structure des sommets de Voronoi */
typedef struct st_vv {
  void    **data;///n:external data
  DOUBLE  point[2];
  short   label;
  short   ind;
  PT_DV    ndvs[3];         //TODO: ndvs replaced by ndvs (neighbour Delaunay Vertices)
  struct st_vv **nvvs[3];  //      nvvs replaced by nvvs (neighbour Voronoi Vertices)
}ST_VV,*VV,**PT_VV;
//ENORME CHANGEMENT: pour rendre dynamique l'allocation des VVs et conserver la structure d'indexation linéaire tab_vv

extern VV ebvv_new(); ///n:pour allouer la mémoire de la structure pour chaque sommet
extern void ebvv_free(); ///n:pour libérer
extern short ebvv_isDispo();
extern void ebvv_angle();//dans ebcalcul.c
extern DOUBLE ebvv_petitAngle();
extern DOUBLE ebvv_grandAngle();
extern void ebvv_order3();//dans ebutil.c
extern DOUBLE ebvv_area();
extern DOUBLE ebvv_perimeter();

/*typedef struct st_arete
{
  PT_DV p_f[3];
  PT_VV p_s;
}ARETE,*PT_ARETE;*/

//VOR struct!!!
typedef struct st_vor {
  int nb_dv_max;
  int nb_vv_max;
  int nb_dv; 
  int nb_dv_total;//nb de point effectif!!!
  int nb_dv_totalIn;//nb de point effectif inside!!!
  int nb_vv;
  DOUBLE center[2];
  DOUBLE size[2];
  DOUBLE centerIn[2];
  DOUBLE sizeIn[2]; //Inside!!!
  PT_DV tab_dv;
  PT_VV tab_vv;
  short list_dispo_ordo;
  PT_LIST list_vvs_dispo;
  PT_LIST list_dvs_dispo;
  DATA_CENTER dc; //dataCenter
  DATA_USER marks; //dataUser marks
  DATA_USER kppv; //dataUser kppv
  CQLS_REXPR marksGen;
}ST_VOR,VOR,*PT_VOR;

extern PT_VOR  ebvor_new();
extern void ebvor_free();
extern void ebvor_initExt();
extern void ebvor_scaleExt();
extern void ebvor_initDataExt();
extern void ebvor_poly();
extern void ebvor_detecte_voisin();
extern void ebvor_primitive_polygone();
extern void ebvor_order_vertices();
extern void ebvor_control_list_vv();
extern void ebvor_control_structure();
extern void ebvor_control_vv();
extern void ebvor_control_dv();
extern void ebvor_check_structure();
extern void ebvor_all_vvs_init_label();
extern void ebvor_vv_init_label();
extern void ebvor_dv_init_boolean();

extern void ebvor_vv_check_label();
extern void ebvor_dv_check_boolean();

extern PT_VV ebvor_recup_vv_dans_tab();
extern PT_DV ebvor_recup_dv_dans_tab();
extern void ebvor_new_vv_dispo();
extern void ebvor_new_dv_dispo();
extern PT_DV ebvor_dv_at();
extern PT_VV ebvor_vv_at();
extern void ebvor_setDataCenter();
extern void ebvor_declareMarks();
extern DOUBLE* ebvor_marksDBL();
extern int* ebvor_marksINT();
extern void ebvor_vv_init_label();
extern void ebvor_nvvs_for_vvList_until_order();
extern void ebvor_dvList_in_vvList();
extern void ebvor_all_dvs_from_dv_at_range();

//SUREMENT INUTILE POUR LE PROJET EBVOR!!!!
typedef struct st_face {
  PT_VV p_s; //sommet 
  short opp; //indice du sommet opposé
}FACE,*PT_FACE;


typedef struct st_face_ext {
  PT_VV p_s; //sommet 
  short opp; //indice du sommet opposé
  PT_VV p_svn; //sommet voisin nouveau
  PT_VV p_sva; //sommet voisin ancien
}FACE_EXT,*PT_FACE_EXT;

//POLY struct!!!
typedef struct st_poly {
  short mode; //0=rien, 1=insertion et -1=suppression
  short etat; //mode courant : 1=apply ou 0=cancel
  PT_VOR vg;
  PT_DV point_courant; //point courant
  
  PT_LIST list_faces_contour;
  PT_LIST list_vvs_supprimes;
  PT_LIST list_vvs_nouveaux;
}POLY,*PT_POLY;


extern PT_POLY ebpoly_new();
extern void ebpoly_free();
extern void ebpoly_vider_lists();
extern void ebpoly_make_ins();
extern void ebpoly_apply_ins();
extern void ebpoly_cancel_ins();
extern void ebpoly_final_ins();

extern void ebpoly_make_sup();
extern void ebpoly_apply_sup();
extern void ebpoly_cancel_sup();
extern void ebpoly_final_sup();

#define ebpoly_apply(self) if(self->mode>0) ebpoly_apply_ins(self); else ebpoly_apply_sup(self)
#define ebpoly_cancel(self) if(self->mode>0) ebpoly_cancel_ins(self); else ebpoly_cancel_sup(self)
#define ebpoly_final(self) if(self->mode>0) ebpoly_final_ins(self); else ebpoly_final_sup(self)
#define ebpoly_init_label(self) eblist_vv_init_label(self->list_vvs_supprimes);eblist_vv_init_label(self->list_vvs_nouveaux)

//Action
extern void ebpoly_vider();
extern int ebpoly_nb_dvs();
extern int ebpoly_nbPoints_inside();

//Util!!!
extern void ebutil_indices_arete();
extern int ebutil_index_vv_voisin();
extern void ebutil_ordered_indexes();

//util for eblist
extern void eblist_vider_dv();
#define eblist_vv_init_label ebvor_vv_init_label
//extern int ebutil_index_dv_opp();

//typedef DOUBLE (*INTERF)(DOUBLE x1[2], DOUBLE x2[2]);
#endif
