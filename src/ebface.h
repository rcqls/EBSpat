#ifndef _ebface_h
#define _ebface_h
#include "ebvor.h"

#define DIM 2
#define VV PT_VV
#define DV PT_DV
#define VG PT_VOR
typedef PT_DV DVs[DIM];    //DVs to temporarily extract del vertices from hf->vv!  
typedef PT_VV VVs[DIM];   //VVs temporarily extract the opposed vor vertices from hf->vv!
typedef PT_VV VV2[2];     //VV2 also matches a directed  half face.
                              //VVs and VV2 differ in d-dimension (d>2)
typedef PT_DV *ptDV;      //for parameter function, DVs becomes a ptDV
typedef PT_VV *ptVV;     // and VVs and VV2 become a ptVV


typedef struct st_half_face {
  void **data;///n:external data (juste in case!)
  VV vv; //sommet 
  short side; //indice du sommet opposé
  short label; //pour tester certain critère
}ST_HALF_FACE,ST_HF,*HALF_FACE,*HF;

extern HALF_FACE ebhf_new(VV,short);
extern void ebhf_free();

//EXTENSION DU PROJET POSSIBLE AVEC CET OBJET QUE J'AIME BEAUCOUP!
//  La généralisation 3d ou plus nécessitera cette structure.
//  On va l'utiliser pour chercher les voisinages d'ordre supérieur 
//  et aussi pour le graphe complet avec portée.

//ebhf is a short term for ebhalfface
typedef struct st_half_face_node {
  HALF_FACE hf; 
  struct st_half_face_node *nhfns[DIM]; //neighbour half face node
  //rmk: struct st_half_face_node * = HALF_FACE_NODE
}ST_HALF_FACE_NODE,ST_HFN,*HALF_FACE_NODE,*HFN;

/* Natural extension
typedef struct st_net {
  void* elt;
  short label; //pour tester certain critère
  struct st_half_face_net *nhf[DIM]; //neighbour half face
}ST_HALF_FACE_NODE_NET,*HALF_FACE_NODE_NET;


*/

//TODO: To place in ebvor.h but here for now because no need in other files!
#define ebvv_nvv_i(vv,i) ((*(vv))->nvvs[i])

//hfn is a short term for half face node
extern HALF_FACE_NODE ebhfn_new(VV,short);
extern void ebhfn_free();

//vv
#define ebhfn_vv(hfn) (hfn->hf->vv)  //vv for hfn
#define ebhfn_vv_(hfn) (*(hfn->hf->vv))

//side
#define ebhfn_side(hfn) (hfn->hf->side) //side for hfn
extern short ebhfn_side_in_vv_of_dv(); //given a dv give the 
extern short ebhfn_side_in_vv_of_vv0();

// index translation:  
#define i_but(i,k) ( (i)<(k) ? (i) : (i)+1 )
#define ebhfn_i_hf2vv(hfn,i)  i_but(i,ebhfn_side(hfn)) //    i:0..DIM-1 (for hdn->nhdn) -> i:0..DIM (for ebhfn_vv(hfn)->nvvs)
//#define ebhfn_i_vv2hf(hfn,i)  ??? => useless!!!
#define ebhfn_i ebhfn_i_hf2vv //default

//dv (i.e. giving side) and dvs (i.e. the half-face vertices => hfvs)
#define ebhfn_dv(hfn) (ebhfn_vv_(hfn)->ndvs[ebhfn_side(hfn)]) //dv is not opposed since it is inside
#define ebhfn_dvs_i(hfn,i)  (ebhfn_vv_(hfn)->ndvs[ebhfn_i(hfn,i)]) // hf_dvs[i] where hf_dvs=dvs of the face
#define ebhfn_hfvs_i  ebhfn_dvs_i
extern void ebhfn_dvs();
#define ebhfn_hfvs  ebhfn_dvs

//opposed vv and other neighbour vvs
#define ebhfn_opp_nvv(hfn) (ebhfn_vv_(hfn)->nvvs[ebhfn_side(hfn)]) // opp_nvv (as opposed nvv)
extern void ebhfn_vv_and_opp_nvv();
#define ebhfn_other_nvvs_i(hfn,i) (ebhfn_vv_(hfn)->nvvs[ebhfn_i(hfn,i)]) // other_nvvs[i] where other_nvvs is explict enough!
extern void ebhfn_other_nvvs();
extern short ebhfn_side_of_opp_nvv();   //side of opposed neigbour vv (from its point of view) 
extern PT_DV ebhfn_dv_of_opp_nvv();  //dv giving the side of opposed neigbour vv ( ... )

//operation of move
extern void ebhfn_go();

//information related to the graph of hfns (notion of side and dv exist too)
//#define ebhfn_opp_nhfn(hfn) (ebhfn_vv_(hfn)->[ebhfn_side(hfn)]) // opp_nvv (as opposed nvv)
//extern void ebhfnGraph_side_of_opp_nhfn();

extern short ebhfnGraph_side_in_hfn_of_dv();
extern short ebhfnGraph_side_in_hfn_of_hfn0();
#define ebhfnGraph_side_of_hfn_in_nhfns_i(hfn,i) ebhfnGraph_side_in_hfn_of_hfn0(hfn->nhfns[i],hfn)

extern void ebhfnGraph_sides_of_two_neighbour_hfns();

///////// half faces network
#define HALF_FACE_GRAPH PT_LIST
#define HFG PT_LIST //for short

extern HFG ebhfg_new_from_dv();
extern HFG ebhfg_new_from_vv();
extern void ebhfg_free();


//////// graph structure => TODO: to develop! 
// Is Haskell better suited to do this kind of job??? That is a good question but without answer now!

/* typedef struct st_eb_node {
  void *node;
  struct st_graph_node *neighbours; //the number of neighbours is dynamically allocated.
} ST_EB_NODE,*EB_NODE;

#define EBLIST PT_LIST

typedef struct st_eb_graph {
  EBLIST nodes; //nodes list
  short d;      //dimension
} ST_EB_GRAPH,*EB_GRAPH;

*/


#endif
