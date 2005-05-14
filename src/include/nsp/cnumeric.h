#ifndef NSP_INC_CNUMERIC
#define NSP_INC_CNUMERIC

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "machine.h"

double nsp_abs_c(const  doubleC *x);
double nsp_arg_c( const doubleC *x);
void nsp_polar_c( double r,double t,doubleC *res);
void nsp_conj_c(const  doubleC *x,doubleC *res);
double nsp_norm_c(const  doubleC *x,doubleC *res);
void nsp_cos_c(const  doubleC *x,doubleC *res);
void nsp_cosh_c(const  doubleC *x,doubleC *res);
void nsp_exp_c(const  doubleC *x,doubleC *res);
void nsp_log_c(const  doubleC *x,doubleC *res);
void nsp_pow_cc(const  doubleC *x,const doubleC *y,doubleC *res);
void nsp_pow_cd( const doubleC *x, double y,doubleC *res);
void nsp_pow_dc( double x, const doubleC *y,doubleC *res);
void nsp_sin_c(const  doubleC *x,doubleC *res);
void nsp_sinh_c(const  doubleC *x,doubleC *res);
void nsp_div_cc( const doubleC *x,doubleC *y,doubleC *res);
void nsp_div_dc( double x,const doubleC *y,doubleC *res);
void nsp_pow_ci(const  doubleC *xe, int y,doubleC *res);
void nsp_sqrt_c(const  doubleC *x,doubleC *res);
void nsp_prod_c( doubleC *x,const doubleC *y);
double nsp_isnan_c(const  doubleC *x);
double nsp_isinf_c(const  doubleC *x);
double nsp_finite_c( const doubleC *x);
void nsp_acos_c( const doubleC *x,doubleC *res);
void nsp_acosh_c(const  doubleC *x,doubleC *res);
void nsp_asin_c( const doubleC *x,doubleC *res);
void nsp_asinh_c(const  doubleC *x,doubleC *res);
void nsp_atan_c(const  doubleC *x,doubleC *res);
void nsp_atanh_c(const  doubleC *x,doubleC *res);
void nsp_ceil_c(const  doubleC *x,doubleC *res);
void nsp_aint_c(const  doubleC *x,doubleC *res);
void nsp_floor_c(const  doubleC *x,doubleC *res);
void nsp_log10_c(const  doubleC *x,doubleC *res);
void nsp_round_c(const  doubleC *x,doubleC *res);
void nsp_signum_c( const doubleC *x,doubleC *res);
void nsp_tan_c(const  doubleC *x,doubleC *res);
void nsp_tanh_c(const  doubleC *x,doubleC *res);

#endif
