#ifndef _R_ebspat_h
#define _R_ebspat_h
#include <Rinternals.h>
#include "CqlsRCom.h"

//from other R_*.c
extern void* C_ebvor_reactivate();
extern void* C_ebpoly_reactivate();
extern void* C_ebmodel_reactivate();
extern void* C_ebmodel_energy_reactivate();
extern void* C_ebfunc_reactivate();
extern void* C_ebcache_reactivate();
extern void* C_ebcacheCompFunc_reactivate();
extern void* C_ebfunc_function_reactivate();
extern void* C_ebsim_reactivate();
extern void* C_ebpseudoExpo_reactivate();

SEXP R_ebvor_new(SEXP args);
SEXP R_ebvor_init(SEXP args);
SEXP R_ebvor_initExt(SEXP args);
SEXP R_ebvor_free(SEXP args);
SEXP R_ebvor_reactivate(SEXP args);
SEXP R_ebvor_marks_set(SEXP args);
SEXP R_ebvor_center_set(SEXP args);
SEXP R_ebvor_centerIn_set(SEXP args);
SEXP R_ebvor_size_set(SEXP args);
SEXP R_ebvor_sizeIn_set(SEXP args);
SEXP R_ebpoly_new(SEXP args);
SEXP R_ebpoly_reactivate(SEXP args);
SEXP R_ebpoly_empty(SEXP args);
SEXP R_ebpoly_ins(SEXP args); //all the insertion process (make, apply, final)
SEXP R_ebpoly_make_ins(SEXP args);//only make insertion
SEXP R_ebpoly_delete_at(SEXP args);//all the suppression process
SEXP R_ebpoly_make_sup(SEXP args);//only make suppression
SEXP R_ebpoly_apply(SEXP args);//only apply (after make or cancel)
SEXP R_ebpoly_cancel(SEXP args);//only cancel (after apply or cancel)
SEXP R_ebpoly_final(SEXP args);//only finalize (no way to go back)!
SEXP R_ebpoly_current_dv(SEXP args);
SEXP R_ebvor_vertex(SEXP args);
SEXP R_ebvor_show_vertex(SEXP args);
SEXP R_ebvor_vorEdge(SEXP args);
SEXP R_ebvor_delEdge(SEXP args);
SEXP R_ebvor_vorGraph(SEXP args);
SEXP R_ebvor_polygon(SEXP args);

/*SEXP R_ebmodel_new(SEXP args);
SEXP R_ebmodel_reactivate(SEXP args);
SEXP R_ebmodel_add_single(SEXP args);
SEXP R_ebmodel_add_energy(SEXP args);
SEXP R_ebmodel_append_model(SEXP args);
SEXP R_ebmodel_show_energyList(SEXP args);
SEXP R_ebmodel_energy_new(SEXP args);*/


SEXP R_ebfunc_new(SEXP args);
SEXP R_ebfunc_free(SEXP args);
SEXP R_ebfunc_reactivate(SEXP args);
//SEXP R_ebfunc_add_single(SEXP args);
SEXP R_ebfunc_add_function(SEXP args);
SEXP R_ebfunc_function_append(SEXP args);
SEXP R_ebfunc_show_functionList(SEXP args);
SEXP R_ebfunc_idTerms(SEXP args);
SEXP R_ebfunc_localEnergy(SEXP args);
SEXP R_ebfunc_globalEnergy(SEXP args);
SEXP R_ebfunc_componentUpdate(SEXP args);
SEXP R_ebfunc_componentShow(SEXP args);
SEXP R_ebfunc_componentGet(SEXP args);
SEXP R_ebcache_reactivate(SEXP args);
SEXP R_ebcache_test(SEXP args);
SEXP R_ebcache_new(SEXP args);
SEXP R_ebcacheSystSampl_init(SEXP args);
SEXP R_ebcacheRandSampl_init(SEXP args);
SEXP R_ebcacheSum_init(SEXP args);
SEXP R_ebcache_free(SEXP args);
SEXP R_ebcache_print(SEXP args);
SEXP R_ebcache_compute(SEXP args);
SEXP R_ebcache_sum_code(SEXP args);
SEXP R_ebcache_matrix(SEXP args);

SEXP R_ebcacheCompFunc_reactivate(SEXP args);
SEXP R_ebcacheCompFunc_test(SEXP args);
SEXP R_ebcacheCompFunc_new(SEXP args);
SEXP R_ebcacheCompFunc_initSystSampl(SEXP args);
SEXP R_ebcacheCompFunc_initRandSampl(SEXP args);
SEXP R_ebcacheCompFunc_initSum(SEXP args);
SEXP R_ebcacheCompFunc_free(SEXP args);
SEXP R_ebcacheCompFunc_print(SEXP args);
SEXP R_ebcacheCompFunc_as_dataframe(SEXP args);

SEXP R_ebfunc_function_new(SEXP args);

SEXP R_ebsim_new(SEXP args);
SEXP R_ebsim_free(SEXP args);
SEXP R_ebsim_reactivate(SEXP args);
//SEXP R_ebsim_genMarks_set(SEXP args);
SEXP R_ebsim_domainIn_set(SEXP args);
SEXP R_ebsim_run(SEXP args);
SEXP R_ebsim_nb_dv(SEXP args);
SEXP R_ebsim_m_set(SEXP args);
SEXP R_ebsim_func_set(SEXP args);
SEXP R_ebsim_poly_set(SEXP args);

/*SEXP R_ebpseudoExpo_new(SEXP args);
SEXP R_ebpseudoExpo_reactivate(SEXP args);
SEXP R_ebpseudoExpo_genMarks_set(SEXP args);
SEXP R_ebpseudoExpo_statex_update(SEXP args);
SEXP R_ebpseudoExpo_statex_get(SEXP args);
SEXP R_ebpseudoExpo_statex_dv(SEXP args);
SEXP R_ebpseudoExpo_pseudo(SEXP args);
SEXP R_ebpseudoExpo_gradient(SEXP args);

SEXP R_ebtest_getVarInEnvir(SEXP args);
SEXP R_ebtest_getVarRObject(SEXP args);*/
#endif



