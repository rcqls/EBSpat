#ifndef _ebstruct_h
#define _ebstruct_h

/* Structure des germes de Delaunay */
typedef struct st_raw_dv {
  void **data;///n:external data
  DOUBLE point[2];
}ST_RAW_DV,*RAW_DV,**PT_RAW_DV;


/* Edge structure */
typedef struct st_edge {
  PT_RAW_DV point1,point2;
}ST_EDGE,*EDGE;


#endif
