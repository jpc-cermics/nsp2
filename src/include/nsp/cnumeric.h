#ifndef SCI_CNUMERIC
#define SCI_CNUMERIC

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

/** XXXX : je fais pour l'instant comme ds sparse **/
/** float.h existe chez Gnu **/
/** pour l'instant je copie le float.h d'un sun **/
/** lapack doit aussi avoir cela **/

#ifndef DBL_MAX
#define DBL_MAX 1.7976931348623157e+308
#endif 
#ifndef DBL_EPSILON 
#define DBL_EPSILON 2.2204460492503131e-16
#endif 
#ifndef DBL_MIN
#define DBL_MIN 2.2250738585072014e-308
#endif 
#ifndef SHRT_MAX
#define SHRT_MAX 32767
#endif 
#ifndef LONG_MAX 
#define LONG_MAX 2147483647L
#endif 

#define  MACHINE_RESOLUTION      DBL_EPSILON
#define  LARGEST_REAL            DBL_MAX
#define  SMALLEST_REAL           DBL_MIN
#define  LARGEST_SHORT_INTEGER   SHRT_MAX
#define  LARGEST_LONG_INTEGER    LONG_MAX

#ifndef M_LOG10E
#define M_LOG10E 0.43429448190325182765
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

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
