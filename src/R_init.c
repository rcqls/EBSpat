#include "R_ebspat.h"

#include <R_ext/Rdynload.h>

void ebfunc_init();

static const R_CMethodDef cMethods[] = {
  {"EBFunc_init",(DL_FUNC) &ebfunc_init,0},
  {NULL,NULL,0}
};

/*static const R_CallMethodDef callMethods[] = {
  {"getvar", (DL_FUNC) &getvar, 3},
  {NULL, NULL, 0}
};*/

static const R_ExternalMethodDef externalMethods[] = {
  //ebvor
  {"EBVor_new",(DL_FUNC) &R_ebvor_new,-1},
  {"EBVor_init",(DL_FUNC) &R_ebvor_init,-1},
  {"EBVor_initExt",(DL_FUNC) &R_ebvor_initExt,-1},
  {"EBVor_free",(DL_FUNC) &R_ebvor_free,-1},
  {"EBGibbs_free",(DL_FUNC) &R_ebvor_free,-1},
  {"EBVor_reactivate",(DL_FUNC) &R_ebvor_reactivate,-1},
  {"EBVor_marks_set",(DL_FUNC) &R_ebvor_marks_set,-1},
  {"EBVor_center_set",(DL_FUNC) &R_ebvor_center_set,-1},
  {"EBVor_centerIn_set",(DL_FUNC) &R_ebvor_centerIn_set,-1},
  {"EBVor_size_set",(DL_FUNC) &R_ebvor_size_set,-1},
  {"EBVor_sizeIn_set",(DL_FUNC) &R_ebvor_sizeIn_set,-1},
  {"EBPoly_new",(DL_FUNC) &R_ebpoly_new,-1},
  {"EBPoly_reactivate",(DL_FUNC) &R_ebpoly_reactivate,-1},
  {"EBPoly_empty",(DL_FUNC) &R_ebpoly_empty,-1}, 
  {"EBPoly_make_ins",(DL_FUNC) &R_ebpoly_make_ins,-1}, 
  {"EBPoly_ins",(DL_FUNC) &R_ebpoly_ins,-1},
  {"EBPoly_make_sup",(DL_FUNC) &R_ebpoly_make_sup,-1}, 
  {"EBPoly_delete_at",(DL_FUNC) &R_ebpoly_delete_at,-1},
  {"EBPoly_apply",(DL_FUNC) &R_ebpoly_apply,-1}, 
  {"EBPoly_cancel",(DL_FUNC) &R_ebpoly_cancel,-1},
  {"EBPoly_final",(DL_FUNC) &R_ebpoly_final,-1},
  {"EBPoly_current_dv",(DL_FUNC) &R_ebpoly_current_dv,-1},
  {"EBVor_vertex",(DL_FUNC) &R_ebvor_vertex,-1},
  {"EBVor_show_vertex",(DL_FUNC) &R_ebvor_show_vertex,-1},
  {"EBVor_vorEdge",(DL_FUNC) &R_ebvor_vorEdge,-1},
  {"EBVor_vorGraph",(DL_FUNC) &R_ebvor_vorGraph,-1},
  {"EBVor_delEdge",(DL_FUNC) &R_ebvor_delEdge,-1},
  {"EBVor_polygon",(DL_FUNC) &R_ebvor_polygon,-1},
  //ebmodel
  //{"ebmodel_new",(DL_FUNC) &R_ebmodel_new,-1},
  //{"ebmodel_reactivate",(DL_FUNC) &R_ebmodel_reactivate,-1},
  //{"ebmodel_setVor",(DL_FUNC) &R_ebmodel_setVor,-1},
  //{"ebmodel_add_single",(DL_FUNC) &R_ebmodel_add_single,-1},
  //{"ebmodel_add_energy",(DL_FUNC) &R_ebmodel_add_energy,-1},
  //{"ebmodel_append_model",(DL_FUNC) &R_ebmodel_append_model,-1},
  //{"ebmodel_show_energyList",(DL_FUNC) &R_ebmodel_show_energyList,-1},
  //{"ebmodel_energy_new",(DL_FUNC) &R_ebmodel_energy_new,-1},
  //ebfunc
  {"EBFunc_new",(DL_FUNC) &R_ebfunc_new,-1},
  {"EBFunc_free",(DL_FUNC) &R_ebfunc_free,-1},
  {"EBFunc_reactivate",(DL_FUNC) &R_ebfunc_reactivate,-1},
  //{"EBfunc_setVor",(DL_FUNC) &R_ebfunc_setVor,-1},
  //{"EBfunc_add_single",(DL_FUNC) &R_ebfunc_add_single,-1},
  {"EBFunc_add_function",(DL_FUNC) &R_ebfunc_add_function,-1},
  {"EBFunc_function_append",(DL_FUNC) &R_ebfunc_function_append,-1},
  {"EBFunc_show_functionList",(DL_FUNC) &R_ebfunc_show_functionList,-1},
  {"EBFunc_idTerms", (DL_FUNC) &R_ebfunc_idTerms,-1},
  {"EBFunc_function_new",(DL_FUNC) &R_ebfunc_function_new,-1}, 
  {"EBFunc_localEnergy",(DL_FUNC) &R_ebfunc_localEnergy,-1}, 
  {"EBFunc_globalEnergy",(DL_FUNC) &R_ebfunc_globalEnergy,-1}, 
  //{"EBFunc_componentUpdate",(DL_FUNC) &R_ebfunc_componentUpdate,-1},
  {"EBFunc_componentShow",(DL_FUNC) &R_ebfunc_componentShow,-1},
  {"EBFunc_componentGet",(DL_FUNC) &R_ebfunc_componentGet,-1},
  {"EBCache_test",(DL_FUNC) &R_ebcache_test,-1},
  {"EBCache_reactivate",(DL_FUNC) &R_ebcache_reactivate,-1},
  {"EBCache_new",(DL_FUNC) &R_ebcache_new,-1},
  {"EBCacheSystSampl_init",(DL_FUNC) &R_ebcacheSystSampl_init,-1},
  {"EBCacheRandSampl_init",(DL_FUNC) &R_ebcacheRandSampl_init,-1},
  {"EBCacheSum_init",(DL_FUNC) &R_ebcacheSum_init,-1},
  {"EBCache_free",(DL_FUNC) &R_ebcache_free,-1},
  {"EBCache_print",(DL_FUNC) &R_ebcache_print,-1},
  {"EBCache_compute",(DL_FUNC) &R_ebcache_compute,-1},
  {"EBCache_sum",(DL_FUNC) &R_ebcache_sum_code,-1},
  {"EBCache_matrix",(DL_FUNC) &R_ebcache_matrix,-1},
  //ebcacheCompFunc
  {"EBCacheCompFunc_reactivate",(DL_FUNC) &R_ebcacheCompFunc_reactivate,-1},
  {"EBCacheCompFunc_new",(DL_FUNC) &R_ebcacheCompFunc_new,-1},
  {"EBCacheCompFunc_initSystSampl",(DL_FUNC) &R_ebcacheCompFunc_initSystSampl,-1},
  {"EBCacheCompFunc_initRandSampl",(DL_FUNC) &R_ebcacheCompFunc_initRandSampl,-1},
  {"EBCacheCompFunc_initSum",(DL_FUNC) &R_ebcacheCompFunc_initSum,-1},
  {"EBCacheCompFunc_free",(DL_FUNC) &R_ebcacheCompFunc_free,-1},
  {"EBCacheCompFunc_print",(DL_FUNC) &R_ebcacheCompFunc_print,-1},
  {"EBCacheCompFunc_as_dataframe",(DL_FUNC) &R_ebcacheCompFunc_as_dataframe,-1},
  //ebsim
  {"EBSim_new",(DL_FUNC) &R_ebsim_new,-1},
  {"EBSim_free",(DL_FUNC) &R_ebsim_free,-1},
  {"EBSim_reactivate",(DL_FUNC) &R_ebsim_reactivate,-1},
//  {"EBSim_genMarks_set",(DL_FUNC) &R_ebsim_genMarks_set,-1},
  {"EBSim_domainIn_set",(DL_FUNC) &R_ebsim_domainIn_set,-1},
  {"EBSim_run",(DL_FUNC) &R_ebsim_run,-1},
  {"EBSim_nb_dv",(DL_FUNC)&R_ebsim_nb_dv,-1},
  {"EBSim_m_set",(DL_FUNC) &R_ebsim_m_set,-1},
  {"EBSim_func_set",(DL_FUNC) &R_ebsim_func_set,-1},
  {"EBSim_poly_set",(DL_FUNC) &R_ebsim_poly_set,-1},
  //ebpseudo
  //{"ebpseudoExpo_new",(DL_FUNC) &R_ebpseudoExpo_new,-1},
  //{"ebpseudoExpo_reactivate",(DL_FUNC) &R_ebpseudoExpo_reactivate,-1},
  //{"ebpseudoExpo_genMarks_set",(DL_FUNC) &R_ebpseudoExpo_genMarks_set,-1},
  //{"ebpseudoExpo_statex_update",(DL_FUNC) &R_ebpseudoExpo_statex_update,-1},
  //{"ebpseudoExpo_statex_get",(DL_FUNC) &R_ebpseudoExpo_statex_get,-1},
  //{"ebpseudoExpo_statex_dv",(DL_FUNC) &R_ebpseudoExpo_statex_dv,-1},
  //{"ebpseudoExpo_pseudo",(DL_FUNC) &R_ebpseudoExpo_pseudo,-1},
  //{"ebpseudoExpo_gradient",(DL_FUNC) &R_ebpseudoExpo_gradient,-1},
  //{"ebpseudoExpo_pseudo_by_statex",(DL_FUNC) &R_ebpseudoExpo_pseudo_by_statex,-1},
  //{"ebpseudoExpo_gradient_by_statex",(DL_FUNC) &R_ebpseudoExpo_gradient_by_statex,-1},
  //test series
  //{"ebtest_getVarInEnvir",(DL_FUNC) &R_ebtest_getVarInEnvir,-1},
  //{"ebtest_getVarRObject",(DL_FUNC) &R_ebtest_getVarRObject,-1},
  {NULL,NULL,0}
};

void R_init_EBSpat(DllInfo *dll) {
  R_registerRoutines(dll,cMethods,NULL,NULL,externalMethods); //callMethods in 2nd position!
  R_useDynamicSymbols(dll, FALSE);

}

