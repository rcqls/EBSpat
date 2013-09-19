#ifndef _ebfunc_h
#define _ebfunc_h
#include "cqlsVector.h"
#include "ebvor.h"

#define SHOW_VECTOR(vect,len,title,i) \
printf("%s=(",title);for(i=0;i<len;i++) printf("%LF%s",vect[i],(i==len-1 ? ")\n" : ","));

////////////////////////////////////////////////////// GENERAL
//class ParamStruct
/*typedef struct st_func_param {
  int nbParam;
  int nbBlockParam;
  int *sizeBlockParam;
  char **keyBlockParam;
} ST_FUNC_PARAM,*FUNC_PARAM;
extern FUNC_PARAM ebfunc_param_new();
extern void ebfunc_param_free();
extern int ebfunc_param_realloc();
extern void ebfunc_param_add_block();
//extern void ebfunc_param_append();
extern DOUBLE* ebfunc_param_export();
extern void ebfunc_param_import();
extern void ebfunc_param_show();*/

//class Info
typedef struct st_func_info {
  int *infos;
  int *infoList;
  int nbInfoList;
} ST_FUNC_INFO,*FUNC_INFO;
extern FUNC_INFO ebfunc_info_new();
extern void ebfunc_info_free();
extern void ebfunc_info_add();
extern int ebfunc_ncol();

////////////////////////////////////////////////////// FUNCTION
//The id(s) of TYPE of FUNC_TERM_???
typedef enum {
FUNC_TYPE_DEL1,
FUNC_TYPE_DEL2,
FUNC_TYPE_DEL3,
FUNC_TYPE_ALL2,
FUNC_TYPE_NNG,
FUNC_TYPE_LAST //=NB de FUNC_TYPE
} FUNC_TYPE;
#define FUNC_TERM_SIZE FUNC_TYPE_LAST

//VECTOR CACHE
typedef struct st_func_vector_cache {
  CQLS_VECTOR_LIST oldList;
  CQLS_VECTOR_LIST list; //newList or the list when one list is needed! 
} ST_FUNC_VECTOR_CACHE, *FUNC_VECTOR_CACHE;

extern FUNC_VECTOR_CACHE ebfunc_vector_cache_new();
extern void ebfunc_vector_cache_free();
extern SEXP ebfunc_vector_cache_lists_to_R_as_named_list();
extern void ebfunc_vector_cache_cumVector();
extern SEXP ebfunc_vector_cache_cumVector_to_R_as_named_list_from_vector();
extern void ebfunc_vector_cache_show();
//COMMON FUNC_TERM_??? 
 
//FUNCTION: in fact potential term
typedef struct st_function {
  //Type of interaction
  FUNC_TYPE id;//id=FUNC_TYPE_???
  //Interaction function
  //char *callR;
  int *infoList;
  int nbInfoList;
  CQLS_REXPR R;//eval R expression for the general purpose
  CQLS_VECTOR vectorCur;//just a pointer to the current vector!
  //local
  CQLS_VECTOR vectorLoc,vectorCompFuncLoc;//to eval components of functional (and save them into a cache)!
  FUNC_VECTOR_CACHE cacheLoc;
  //global
  CQLS_VECTOR vectorGlob,vectorCompFuncGlob;
  FUNC_VECTOR_CACHE cacheGlob;
} ST_FUNCTION,*FUNCTION;

extern FUNCTION ebfunc_function_new();
extern void ebfunc_function_free();
extern void ebfunc_function_show();
extern void ebfunc_function_init();
extern void ebfunc_function_addVector();
extern void ebfunc_function_addInfo();
extern void ebfunc_function_setFormula();
extern void ebfunc_function_setInfo();
extern void ebfunc_function_setVector();

typedef struct st_func_term_ {
  FUNCTION function;
  FUNC_INFO info;
}*FUNC_TERM_;
extern void ebfunc_term_new_();
extern void ebfunc_term_free_();
extern void ebfunc_term_add_function();

typedef const char* FUNC_INFO_KEY;

//////////////////////////////////////// Singleton
#define FUNC_SINGLE_KEY "Single"
extern void ebfuncExpo_functionSingle_componentUpdate();

//////////////////////////////////////// DEL1
//The id(s) of INFO for DEL1!
typedef enum {
  INFO_DEL1_ID,//Id
  INFO_DEL1_X,//Coordinate
  INFO_DEL1_V,//Marks
  INFO_DEL1_A,//Area
  INFO_DEL1_LAST
} INFO_DEL1_TYPE;

static const FUNC_INFO_KEY INFO_KEY_DEL1[]={"id","x","v","a"};
static const DATA_TYPE INFO_DATATYPE_DEL1[]={DATA_INT,DATA_DBL,DATA_NULL,DATA_DBL};

//Info on the component needed to measure the function!
typedef struct st_component_termDel1 {
  PT_DV point1;
  FUNC_INFO info;//one by term because of ebfunc_termDel1_updateInfo!
} ST_COMPONENT_TERM_DEL1,*COMPONENT_TERM_DEL1;
extern COMPONENT_TERM_DEL1 ebcomponent_termDel1_new();
extern void ebcomponent_termDel1_free();
extern void ebcomponent_termDel1_set();
extern void ebcomponent_termDel1_updateInfo();

//A PRIORI UN SEUL PAR FUNC VOIRE PAR SESSION
//CAR APRES CHANGEMENT DE interList LA MISE A JOUR DE FUNC_INFO EST RAPIDE!
//Delaunay Edge Func term
typedef struct st_func_termDel1 {
  FUNCTION function;//list of functionDel1
  FUNC_INFO info;
  COMPONENT_TERM_DEL1 component;
} ST_FUNC_TERM_DEL1,*FUNC_TERM_DEL1;

extern FUNC_TERM_DEL1 ebfunc_termDel1_new();
extern void ebfunc_termDel1_free();
extern DOUBLE ebfunc_termDel1_localEnergy();
extern void ebfunc_termDel1_componentLocalUpdate();
extern DOUBLE ebfunc_termDel1_globalEnergy();
extern void ebfunc_termDel1_componentGlobalUpdate();

//////////////////////////////////////// DEL2
//The id(s) of INFO for DEL2!
typedef enum {
  INFO_DEL2_ID,//Vector of ids
  INFO_DEL2_X,//Vector of Coordinates
  INFO_DEL2_V,//Vector of two marks
  INFO_DEL2_A,//Vector of two areas
  INFO_DEL2_L2,//Square of edge length
  INFO_DEL2_L,//Edge length
  INFO_DEL2_OL2,//Square of orthogonal edge length
  INFO_DEL2_OL,//Orthogonal edge length
  INFO_DEL2_DA,//Difference of two areas
  INFO_DEL2_LAST
} INFO_DEL2_TYPE;

static const FUNC_INFO_KEY INFO_KEY_DEL2[]={"id","x","v","a","l2","l","ol2","ol","da"};
static const DATA_TYPE INFO_DATATYPE_DEL2[]={DATA_INT,DATA_NULL,DATA_NULL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL};


//Info on the component needed to measure the function!
typedef struct st_component_termDel2 {
  PT_DV point1;
  PT_DV point2;
  PT_VV sommet;
  PT_VV sommetv;
  FUNC_INFO info;//one by term because of ebfunc_termDel2_updateInfo!
} ST_COMPONENT_TERM_DEL2,*COMPONENT_TERM_DEL2;
extern COMPONENT_TERM_DEL2 ebcomponent_termDel2_new();
extern void ebcomponent_termDel2_free();
extern void ebcomponent_termDel2_set();
extern void ebcomponent_termDel2_setFromSommet();
extern void ebcomponent_termDel2_updateInfo();


//A PRIORI UN SEUL PAR FUNC VOIRE PAR SESSION
//CAR APRES CHANGEMENT DE interList LA MISE A JOUR DE FUNC_INFO EST RAPIDE!
//Delaunay Edge Func term
typedef struct st_func_termDel2 {
  FUNCTION function;
  FUNC_INFO info;
  COMPONENT_TERM_DEL2 component;
  short order;
} ST_FUNC_TERM_DEL2,*FUNC_TERM_DEL2;

extern FUNC_TERM_DEL2 ebfunc_termDel2_new();
extern void ebfunc_termDel2_free();
extern DOUBLE ebfunc_termDel2_localEnergy();
extern void ebfunc_termDel2_componentLocalUpdate();
extern DOUBLE ebfunc_termDel2_globalEnergy();
extern void ebfunc_termDel2_componentGlobalUpdate();

//////////////////////////////////////// DEL3
//The id(s) of INFO for DEL3!
typedef enum {
  INFO_DEL3_ID,//Vector of ids
  INFO_DEL3_X,//Vector of Coordinates
  INFO_DEL3_V,//Vector of marks
  INFO_DEL3_A,//Vector of 3 areas
  INFO_DEL3_TA,//area of the triangle
  INFO_DEL3_TP,//perimeter of the triangle
  INFO_DEL3_C,//Coordinates of the center
  INFO_DEL3_R2,//radius^2
  INFO_DEL3_R,//radius
  INFO_DEL3_SA,//Smallest Angle
  INFO_DEL3_GA,//Greatest Angle
  INFO_DEL3_LAST
} INFO_DEL3_TYPE;

static const FUNC_INFO_KEY INFO_KEY_DEL3[]={"id","x","v","a","ta","tp","c","r2","r","sa","ga"};
static const DATA_TYPE INFO_DATATYPE_DEL3[]={DATA_INT, DATA_NULL,DATA_NULL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL};

//Info on the component needed to measure the function!
typedef struct st_component_termDel3 {
  PT_DV point1;
  PT_DV point2;
  PT_DV point3;
  PT_VV sommet;
  FUNC_INFO info;
} ST_COMPONENT_TERM_DEL3,*COMPONENT_TERM_DEL3;
extern COMPONENT_TERM_DEL3 ebcomponent_termDel3_new();
extern void ebcomponent_termDel3_free();
extern void ebcomponent_termDel3_set();
extern void ebcomponent_termDel3_updateInfo();

//A PRIORI UN SEUL PAR FUNC VOIRE PAR SESSION
//CAR APRES CHANGEMENT DE interList LA MISE A JOUR DE FUNC_INFO EST RAPIDE!
//Delaunay Edge Func term
typedef struct st_func_termDel3 {
  FUNCTION function;
  FUNC_INFO info;
  COMPONENT_TERM_DEL3 component;
} ST_FUNC_TERM_DEL3,*FUNC_TERM_DEL3;

extern FUNC_TERM_DEL3 ebfunc_termDel3_new();
extern void ebfunc_termDel3_free();
//extern void ebfunc_termDel3_setInfo();
extern DOUBLE ebfunc_termDel3_localEnergy();
extern void ebfunc_termDel3_componentLocalUpdate();
extern DOUBLE ebfunc_termDel3_globalEnergy();
extern void ebfunc_termDel3_componentGlobalUpdate();
//////////////////////////////////////// ALL2
//The id(s) of INFO for ALL2!
typedef enum {
  INFO_ALL2_ID,//Vector of ids
  INFO_ALL2_X,//Coordinate
  INFO_ALL2_V,//Marks
  INFO_ALL2_L2,//Square length
  INFO_ALL2_L,//Length
  INFO_ALL2_LAST
} INFO_ALL2_TYPE;

static const FUNC_INFO_KEY INFO_KEY_ALL2[]={"id","x","v","l2","l"};
static const DATA_TYPE INFO_DATATYPE_ALL2[]={DATA_INT,DATA_NULL,DATA_NULL,DATA_DBL,DATA_DBL};

//Info on the component needed to measure the function!
typedef struct st_component_termAll2 {
  PT_DV point1;//maybe useful when dealing with difference between Voronoi areas
  PT_DV point2;
  FUNC_INFO info;//one by term because of ebfunc_termAll2_updateInfo!
} ST_COMPONENT_TERM_ALL2,*COMPONENT_TERM_ALL2;
extern COMPONENT_TERM_ALL2 ebcomponent_termAll2_new();
extern void ebcomponent_termAll2_free();
extern void ebcomponent_termAll2_set();
extern void ebcomponent_termAll2_updateInfo();

//A PRIORI UN SEUL PAR FUNC VOIRE PAR SESSION
//CAR APRES CHANGEMENT DE interList LA MISE A JOUR DE FUNC_INFO EST RAPIDE!
typedef struct st_func_termAll2 {
  FUNCTION function;
  FUNC_INFO info;
  COMPONENT_TERM_ALL2 component;
  DOUBLE range;
} ST_FUNC_TERM_ALL2,*FUNC_TERM_ALL2;

extern FUNC_TERM_ALL2 ebfunc_termAll2_new();
extern void ebfunc_termAll2_free();
extern DOUBLE ebfunc_termAll2_localEnergy();
extern void ebfunc_termAll2_componentLocalUpdate();
extern DOUBLE ebfunc_termAll2_globalEnergy();
extern void ebfunc_termAll2_componentGlobalUpdate();

//////////////////////////////////////// NNG
//The id(s) of INFO for NNG!
typedef enum {
  INFO_NNG_ID,//Vector of ids
  INFO_NNG_X,//Vector of Coordinates
  INFO_NNG_V,//Vector of two marks
  INFO_NNG_A,//Vector of two areas
  INFO_NNG_L2,//Square of edge length
  INFO_NNG_L,//Edge length
  INFO_NNG_DA,//Difference of two areas
  INFO_NNG_LAST
} INFO_NNG_TYPE;

static const FUNC_INFO_KEY INFO_KEY_NNG[]={"id","x","v","a","l2","l","da"};
static const DATA_TYPE INFO_DATATYPE_NNG[]={DATA_INT,DATA_NULL,DATA_NULL,DATA_DBL,DATA_DBL,DATA_DBL,DATA_DBL};



//Info on the component needed to measure the function!
typedef struct st_component_termNNG {
  PT_DV point1;
  PT_DV point2;
  PT_VV sommet;
  PT_VV sommetv;
  FUNC_INFO info;//one by term because of ebfunc_termNNG_updateInfo!
} ST_COMPONENT_TERM_NNG,*COMPONENT_TERM_NNG;
extern COMPONENT_TERM_NNG ebcomponent_termNNG_new();
extern void ebcomponent_termNNG_free();
extern void ebcomponent_termNNG_set();
extern void ebcomponent_termNNG_setFromSommet();
extern void ebcomponent_termNNG_updateInfo();


//A PRIORI UN SEUL PAR FUNC VOIRE PAR SESSION
//CAR APRES CHANGEMENT DE interList LA MISE A JOUR DE FUNC_INFO EST RAPIDE!
//Delaunay Edge Func term
typedef struct st_func_termNNG {
  FUNCTION function;
  FUNC_INFO info;
  COMPONENT_TERM_NNG component;
  short order;
} ST_FUNC_TERM_NNG,*FUNC_TERM_NNG;

extern FUNC_TERM_NNG ebfunc_termNNG_new();
extern void ebfunc_termNNG_free();
extern DOUBLE ebfunc_termNNG_localEnergy();
extern void ebfunc_termNNG_componentLocalUpdate();
extern DOUBLE ebfunc_termNNG_globalEnergy();
extern void ebfunc_termNNG_componentGlobalUpdate();



//INFO_TYPE common!!!
static const FUNC_INFO_KEY* INFO_KEY[]={
  INFO_KEY_DEL1,
  INFO_KEY_DEL2,
  INFO_KEY_DEL3,
  INFO_KEY_ALL2,
  INFO_KEY_NNG,
};
static int INFO_LAST[]={
  INFO_DEL1_LAST,
  INFO_DEL2_LAST,
  INFO_DEL3_LAST,
  INFO_ALL2_LAST,
  INFO_NNG_LAST,
};
 
////////////////////////////////////////////////////// FUNC
//Terme Energie
typedef struct st_func_termWrap {
  FUNC_TERM_ term;
  void (*free)(void*);
  DOUBLE (*localEnergy)(void*,PT_POLY);
  DOUBLE (*globalEnergy)(void*,PT_POLY);
  void (*componentLocalUpdate)(void*,PT_POLY);//component func side
  void (*componentGlobalUpdate)(void*,PT_POLY);
} ST_FUNC_TERM_WRAP,*FUNC_TERM_WRAP;
//extern int ebfunc_termWrap_nbParam();

typedef enum {
  FUNC_NORMAL,
  FUNC_EXPO,
  FUNC_MIXED,
} FUNC_KIND;

//Func
typedef struct st_func {
  FUNC_KIND kind;//0=normal,1=expo,2=mixed!!! => USELESS NOW! DO NOT REMOVE IF USEABLE LATER FOR OTHER STUFF!
  int local; //-1=global, 0=(both), 1=(local)
  //int single;//=0 -> no single, =1 -> single
  //fields related to function terms (wrapper only!)
  FUNC_TERM_WRAP *terms;
  int nbTerms;
  int *termList;
  int nbTermList;
  int *termTypeList;
  //envir
  CQLS_R envR;
  //fields related to the stat exhaustives
  int nbComponent; //USELESS AS BELOW!
  DOUBLE *component;//internal array => USELESS NOW, I GUESS!
} ST_FUNC,*FUNC;
extern FUNC ebfunc_new();
extern void ebfunc_free();
extern void ebfunc_init();
extern SEXP ebfunc_eval1();
//extern void ebfunc_add_single();
//extern void ebfunc_init_single();
extern DOUBLE ebfunc_get_single();
extern void ebfunc_add_function();
extern void ebfunc_append_func();
extern void ebfunc_show_functionList();
//extern void ebfunc_update_funcParam();
extern DOUBLE ebfunc_localEnergy();
extern DOUBLE ebfunc_globalEnergy();
extern void ebfunc_componentCacheNew();
extern void ebfunc_componentCacheFree();
extern void ebfunc_componentCacheUpdate();
extern void ebfunc_componentCacheShow();
extern SEXP ebfunc_componentCacheGet();
extern SEXP ebfunc_componentCacheGetFunc();

#define EBFuncEnvName ".funcEnv"
static CQLS_R EBFUNC_R=NULL;
////////////////////////////////////////////////////// SIM
//Simulateur!!!
typedef struct st_sim {
  DOUBLE center[2];
  DOUBLE size[2],area;
  int nb_dv_totalExt;//nb points  à l'extérieur
  int m;
  FUNC func;
  DOUBLE *param;
  PT_POLY poly;
  //EBMarkGen genMarks;
  DOUBLE (*local_energy)(void*,PT_POLY);
} ST_SIM,*SIM;

extern SIM ebsim_new();
extern void ebsim_free();
extern void ebsim_run();
extern void ebsim_domaineIn_set();

#endif
