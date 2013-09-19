#include "ebface.h"


/************* Half Face ******************/
HF ebhf_new(VV vv,short side) {
  HF self;
  
  self=(HF)Calloc(1,ST_HF);
  self->vv=vv;
  self->side=side;
  return self;
}

void ebhf_free(HF self) {
  if(self!=(HF)NULL) {
    //free the data field
    Free(self);
    self=(HF)NULL;
  }
}


/************ Half Face Node ************/

HFN ebhfn_new(VV vv,short side) {
  HFN self;
  
  self=(HFN)Calloc(1,ST_HFN);
  self->hf=ebhf_new(vv,side);
  return self;
}

void ebhfn_free(HFN self) {///n
  PT_LIST l,pt;

  if(self!=(HFN)NULL) {
    //free half face
    Free(self->hf);
    //free the data field
    Free(self);
    self=(HFN)NULL;
  }
}


short ebhfn_side_in_vv_of_dv(VV vv,DV dv) {
  short i;
  
  for(i=0;i<DIM;i++) if(dv==((*vv)->ndvs[i])) return i;
  return -1;//fails to find side
}

short ebhfn_side_in_vv_of_vv0(VV vv,VV vv0) {
  short i;

  for(i=0;i<DIM;i++) if(vv0==((*vv)->nvvs[i])) return i;
  return -1;
}

//dvs is the result to be modified

void ebhfn_dvs(HFN self, DVs dvs) {//dvs is the result to be modified
  short i;

  for(i=0;i<DIM-1;i++) dvs[i]=ebhfn_dvs_i(self,i);  
  
}

void ebhfn_other_nvvs(HFN self, VVs onvvs) {//nvvs is the result to be modified
  short i;

  for(i=0;i<DIM-1;i++) onvvs[i]=ebhfn_other_nvvs_i(self,i);  
  
}

void ebhfn_vv_and_opp_nvv(HFN self, VV2 vv2) {//vvs is the result to be modified
  short i;

  vv2[0]=ebhfn_vv(self);
  vv2[1]=ebhfn_opp_nvv(self);
  
}


short ebhfn_side_of_opp_nvv(HFN self) {
  
  return ebhfn_side_in_vv_of_vv0( ebhfn_opp_nvv(self), ebhfn_vv(self) );
}

PT_DV ebhfn_dv_of_opp_nvv(HFN self) { //the dv after one go!
  
  return (*ebhfn_opp_nvv(self))->ndvs[ebhfn_side_of_opp_nvv(self)];
}


//work in any dimension!
short ebhfn_is_linked_when_go(HFN self) {//return the side of the side of nhfns matching opp_dv and -1 otherwise! 
  DVs dvs;
  int k,i;
  DV odv=ebhfn_dv_of_opp_nvv(self);
  

  for(k=0;k<DIM;k++) {
    ebhfn_dvs(self->nhfns[k],dvs);
    for(i=0;i<DIM;i++) if(dvs[i]==odv) break;
    if(i<DIM) break;
  }
  if(i==DIM) return(-1);
  return(k);
}

void ebhfn_go(HFN self) {//have to work for n-d!!!
  short linked,i,side_hfn_i,side_ovv;
  HFN hfn,*hfnAry;
  VV ovv=ebhfn_opp_nvv(self);
  DV dv;

  linked=ebhfn_is_linked_when_go(self);
  if(linked>-1) {//

  } else {
    //run over nhfns of self
    for(i=0;i<DIM-1;i++) {
      hfn=ebhfn_new(ovv, -1);
    }
  }

  //NEW TRY
  for(i=0;i<DIM-1;i++) {
    dv=ebhfn_dvs_i(self,i); //for each dvs_i
    side_hfn_i=ebhfnGraph_side_of_hfn_in_nhfns_i(self,i); //find side of the neighbour nhfns[i]
    side_ovv=ebhfn_side_in_vv_of_dv(ovv,dv);
    //creation of 
    //hfnAry[side_ovv]=
  }
}



short ebhfnGraph_side_in_hfn_of_dv(HFN self,DV dv) {
  short i;
  
  for(i=0;i<DIM-1;i++) if(dv==ebhfn_dvs_i(self,i)) return i;
  return -1;//fails to find side
}

short ebhfnGraph_side_in_hfn_of_hfn0(HFN self,HFN hfn0) {
  short i;

  for(i=0;i<DIM;i++) if(hfn0==self->nhfns[i]) return i;
  return -1;
}


//find in the hfnGraph both sides of two neighbour hfn (in order to connect them together)
void ebhfnGraph_sides_of_two_neighbour_hfns(HFN hfn0,HFN hfn1,int sides[2]) {
  int i,j,is0,is1;
  DVs dvs0,dvs1;

  ebhfn_dvs(hfn0,dvs0);
  ebhfn_dvs(hfn1,dvs1);
#ifdef check_ebhfn_sides_of
  sides[0]=-1;sides[1]=-1;
#endif   
  for(i=0;i<DIM-1;i++) {
    is0=0;is1=0;
    for(j=0;j<DIM-1;j++) {
      if(dvs0[i]==dvs1[j]) is0++; 
      if(dvs1[i]==dvs0[j]) is1++;
    }
    if(!is0) {//no other dv of dvs1 matches dvs0[i]!
#ifdef check_ebhfn_sides_of
      if(sides[0]>-1) Rprintf("Error: too many sides found!");      
#endif
      sides[0]=i;
    }
    if(!is1) {//no other dv of dvs0 matches dvs1[i]!
#ifdef check_ebhfn_sides_of
      if(sides[1]>-1) Rprintf("Error: too many sides found!");      
#endif
      sides[1]=i;
    }
  }
}

//used twice in two different ways
void ebhfnGraph_init_all_hfns_from(VV vv,short exceptSide,HFN *hfns) {
  short i,j;

  //create the new hfns;
  for(i=0;i<DIM;i++) if(i!=exceptSide) hfns[i]=ebhfn_new(vv,i); else hfns[i]=(HFN)NULL;
  //connect them
  for(i=0;i<DIM;i++) for(j=0;j<DIM-1;j++) (hfns[i])->nhfns[j]=hfns[i_but(j,i)];

}


/************ Half Face Network ************/

//new from del vertex
HFG ebhfg_new_from_dv(DV dv,VG vg) {
  HFG self=NULL;
  int nb,i=0,ii,k;
  VV *vvAry; //Ary for array
  HFN *hfnAry;
  PT_LIST pt,nvvs=NULL; //nvvs (neighbour Voronoi Vertices) often called list_polygone
  VVs onvvs;

  
  //1) get the neighbourhood of dv
  ebvor_primitive_polygone(vg,dv,&nvvs,&nb);
  hfnAry=(HFN*)Calloc(nb,HFN);  //reasonable since eblist_vider was used at the end!
  vvAry=(VV*)Calloc(nb,VV);  //moreover, the length is not too big!
  //2) 1rst run: creation of the associated hf to vv
  while(nvvs!=NULL) {
    vvAry[i]=(VV)eblist_recup_tete(&nvvs);//list nvvs cleaned!
    hfnAry[i]=ebhfn_new(vvAry[i],ebhfn_side_in_vv_of_dv(vvAry[i],dv));
    //hf and vv paired!
    eblist_ins_tete(self,hfnAry[i]);
    i++;
  }
  //3) 2nd run: connect them together reassigning the neighbourhood of 
  for(i=0;i<nb;i++) {
    ebhfn_other_nvvs(hfnAry[i],onvvs); //vvs are the right vv!
    for(k=0;k<DIM;k++) {      
      for(ii=0;ii<nb;ii++) 
        if(vvAry[ii]==onvvs[k]) break; //the right ii of the right vv
      (hfnAry[i]->nhfns)[k]=hfnAry[ii];     //which corresponds to the right neighbour (i.e. nhf[k]=hf[ii])                    
    }
  }
  Free(hfnAry);Free(vvAry);
  return self;
}

//init from vor vertex which can be use to init from a new point
HFG ebhfg_new_from_vv(VV vv,VG vg) {
  HFG self=NULL;
  HFN *hfnAry;
  short i;

  hfnAry=(HFN*)Calloc(DIM,HFN);
  ebhfnGraph_init_all_hfns_from(vv,-1,hfnAry);
  for(i=0;i<DIM;i++) eblist_ins_tete(self,hfnAry[i]); 
  Free(hfnAry);
}

void ebhfg_free(HFG self) {
  HFN hfn;

  while(self!=(HFG)NULL) {
    hfn=(HFN)eblist_recup_tete(&self);
    ebhfn_free(hfn);
  } 
}
