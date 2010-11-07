#ifndef __SCICOS_BLOCKS__ 
#define __SCICOS_BLOCKS__ 

/* 
 * block prototypes and block table 
 */

#define scicos_args_base  int *flag__, int *nevprt, const double *t, double *xd, double *x, \
    int *nx, double *z__, int *nz, double *tvec, int *ntvec,		\
    double *rpar, int *nrpar, int *ipar, int *nipar 

/* flag  nclock t    xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar  intabl  ni  outabl no */
#define scicos_args_F0  scicos_args_base, double *u, int *nu, double *y, int *ny 

/* flag  nclock t    xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar  u1 n1 u2 n2 .... */
#define scicos_args_F  scicos_args_base, double *uy1, int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3, \
    double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,	\
    int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8,		\
    double *uy9, int *nuy9, double *uy10, int *nuy10, double *uy11, int *nuy11,double *uy12, int *nuy12, \
    double *uy13, int *nuy13, double *uy14, int *nuy14, double *uy15, int *nuy15,double *uy16, int *nuy16, \
    double *uy17, int *nuy17, double *uy18, int *nuy18

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar    args_in sz_in, n_in  args_out sz_out, n_out  */
#define scicos_args_F2 scicos_args_base, double **inptr, int *insz, int *nin, double **outptr, int *outsz, int *nout

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar   args_in sz_in, n_in  args_out sz_out, n_out g ng */
#define scicos_args_F2z scicos_args_F2 , double *g , int *ng

/* flag  nclock t residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar */
#define scicos_args_base_i  int *flag__, int *nevprt,const double *t,double *res, double *xd, double *x, \
    int *nx, double *z__, int *nz, double *tvec, int *ntvec,		\
    double *rpar, int *nrpar, int *ipar, int *nipar 

/* flag  nclock t residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar u1 nu1 u2 nu2 .... */
#define scicos_args_Fi scicos_args_base_i,  double *uy1, int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3, \
    double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,	\
    int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8,		\
    double *uy9, int *nuy9, double *uy10, int *nuy10, double *uy11, int *nuy11,double *uy12, int *nuy12, \
    double *uy13, int *nuy13, double *uy14, int *nuy14, double *uy15, int *nuy15,double *uy16, int *nuy16, \
    double *uy17, int *nuy17, double *uy18, int *nuy18 

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar    args_in sz_in, n_in  args_out sz_out, n_out  */
#define scicos_args_Fi2 scicos_args_base_i, double **inptr, int *insz, int *nin, double **outptr, int *outsz, int *nout

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar   args_in sz_in, n_in  args_out sz_out, n_out g ng */
#define scicos_args_Fi2z scicos_args_Fi2, double *g , int *ng

#define scicos_args_Fm1 int *flag__, int *nevprt, int *ntvec, double *rpar, int *nrpar, int *ipar, int *nipar, double *u, int *nu 

typedef void (*ScicosF0) (scicos_args_F0);
typedef void (*ScicosF) (scicos_args_F); 
typedef void (*ScicosF2) (scicos_args_F2);
typedef void (*ScicosF2z) (scicos_args_F2z);
typedef void (*ScicosFi) (scicos_args_Fi);
typedef void (*ScicosFi2) (scicos_args_Fi2);
typedef void (*ScicosFi2z) (scicos_args_Fi2z);
typedef void (*ScicosFm1) (scicos_args_Fm1);
typedef void (*ScicosF4) (scicos_block *block,int flag);

typedef struct _scicos_block_table scicos_block_table ;

struct _scicos_block_table  {
  char *name;
  ScicosF fonc;
};

extern int scicos_affich_block(scicos_args_F0);
extern int scicos_affich2_block(scicos_args_F0);
extern int scicos_bound (scicos_args_F0);
extern int scicos_constraint_block(scicos_args_Fi);
extern int scicos_csslti_block(scicos_args_F0);
extern int scicos_cst_block(scicos_args_F);
extern int scicos_delay_block(scicos_args_F0);
extern int scicos_delayv_block(scicos_args_F);
extern int scicos_diff_block(scicos_args_Fi);
extern int scicos_dlradp_block(scicos_args_F0);
extern int scicos_dollar_block(scicos_args_F0);
extern int scicos_dsslti_block(scicos_args_F0);
extern int scicos_eselect_block(scicos_args_Fm1);
extern int scicos_evscpe_block(scicos_args_F0);
extern int scicos_evtdly_block(scicos_args_F0);
extern int scicos_exp_block(scicos_args_F0);
extern int scicos_for_block(scicos_args_F0);
extern int scicos_fscope_block(scicos_args_F);
extern int scicos_fsv_block(scicos_args_F0);
extern int scicos_gensin_block(scicos_args_F0);
extern int scicos_gensin_block(scicos_args_F0);
extern int scicos_gensin_test (scicos_args_F0);
extern int scicos_gensqr_block(scicos_args_F0);
extern int scicos_hlt_block(scicos_args_F0);
extern int scicos_ifthel_block(scicos_args_Fm1);
extern int scicos_integr_block(scicos_args_F0);
extern int scicos_intplt_block(scicos_args_F0);
extern int scicos_intpol_block(scicos_args_F0);
extern int scicos_intrp2_block(scicos_args_F);
extern int scicos_intrpl_block(scicos_args_F0);
extern int scicos_inv_block(scicos_args_F0);
extern int scicos_iocopy_block(scicos_args_F0);
extern int scicos_log_block(scicos_args_F0);
extern int scicos_lookup_block(scicos_args_F0);
extern int scicos_lsplit_block(scicos_args_F0);
extern int scicos_lsplit_block(scicos_args_F0);
extern int scicos_lusat_block(scicos_args_F);
extern int scicos_max_block(scicos_args_F0);
extern int scicos_memo_block(scicos_args_F0);
extern int scicos_mfclck_block(scicos_args_F0);
extern int scicos_min_block(scicos_args_F0);
extern int scicos_mscope_block(scicos_args_F0);
extern int scicos_mux_block(scicos_args_F);
extern int scicos_demux_block(scicos_args_F);
extern int scicos_pload_block( scicos_args_F0);
extern int scicos_pow_block(scicos_args_F0);
extern int scicos_qzcel_block(scicos_args_F0);
extern int scicos_qzflr_block(scicos_args_F0);
extern int scicos_qzrnd_block(scicos_args_F0);
extern int scicos_qztrn_block(scicos_args_F0);
extern int scicos_rnd_block(scicos_args_F0);
extern int scicos_samphold_block(scicos_args_F0);
extern int scicos_sawtth_block(scicos_args_F0);
extern int scicos_scope_block(scicos_args_F);
extern int scicos_scopxy_block(scicos_args_F0);
extern int scicos_scoxy_block(scicos_args_F0);
extern int scicos_sel_block(scicos_args_F0);
extern int scicos_sinblk_block(scicos_args_F0);
extern int scicos_sqr_block(scicos_args_F0);
extern int scicos_sum2_block(scicos_args_F);
extern int scicos_sum3_block(scicos_args_F);
extern int scicos_tanblk_block(scicos_args_F0);
extern int scicos_tcslti_block(scicos_args_F);
extern int scicos_tcsltj_block(scicos_args_F0);
extern int scicos_timblk_block(scicos_args_F0);
extern int scicos_trash_block(scicos_args_F0);
extern int scicos_zcross_block(scicos_args_F0);
extern int scicos_integr_block(scicos_args_F0);
extern int scicos_intplt_block(scicos_args_F0);
extern void scicos_abs_block(scicos_args_F0);
extern void scicos_andlog_block(scicos_args_F0);
extern void scicos_bidon_block(scicos_args_F0);
extern void scicos_cdummy_block(scicos_args_F0);
extern void scicos_cosblk_block(scicos_args_F0);
extern void scicos_dband_block(scicos_args_F0);
extern void scicos_gain_block(scicos_args_F0);
extern void scicos_plus_block(scicos_args_F2);
extern void scicos_prod_block(scicos_args_F2);
extern void scicos_readau_block(scicos_args_F2);
extern void scicos_readc_block(scicos_args_F2);
extern void scicos_readf_block(scicos_args_F0);
extern void scicos_relay_block(scicos_args_F2);
extern void scicos_plusblk(scicos_args_F2);
extern void scicos_selector_block(scicos_args_F2);
extern void scicos_slider_block(scicos_args_F0);
extern void scicos_sum_block(scicos_args_F2);
extern void scicos_switchn_block(scicos_args_F2);
extern void scicos_writeau_block(scicos_args_F2);
extern void scicos_writec_block(scicos_args_F2);
extern void scicos_writef_block(scicos_args_F0);
extern void scicos_zcross2_block(scicos_args_F0);

extern void scicos_absolute_value_block(scicos_block *block,int flag); 
extern void scicos_acos_block(scicos_block *block,int flag);
extern void scicos_acosh_block(scicos_block *block,int flag);
extern void scicos_asin_block(scicos_block *block,int flag);
extern void scicos_asinh_block(scicos_block *block,int flag);
extern void scicos_atan_block(scicos_block *block,int flag);
extern void scicos_atanh_block(scicos_block *block,int flag);
extern void scicos_tanh_block(scicos_block *block,int flag);
extern void scicos_tan_block(scicos_block *block,int flag);
extern void scicos_sin_block(scicos_block *block,int flag);
extern void scicos_sinh_block(scicos_block *block,int flag);
extern void scicos_backlash_block(scicos_block *block,int flag);
extern void scicos_cos_block(scicos_block *block,int flag);
extern void scicos_cosh_block(scicos_block *block,int flag);
extern void scicos_deadband_block(scicos_block *block,int flag);
extern void scicos_deriv_block(scicos_block *block,int flag);
extern void scicos_extractor_block(scicos_block *block,int flag);
extern void scicos_gainblk_block(scicos_block *block,int flag);
extern void scicos_time_delay_block(scicos_block *block,int flag);
extern void scicos_variable_delay_block(scicos_block *block,int flag);
extern void scicos_step_func_block(scicos_block *block,int flag);
extern void scicos_signum_block(scicos_block *block,int flag);
extern void scicos_summation_block(scicos_block *block,int flag);
extern void scicos_switch2_block(scicos_block *block,int flag);
extern void scicos_satur_block(scicos_block *block,int flag);
extern void scicos_logicalop_block(scicos_block *block,int flag);
extern void scicos_multiplex_block(scicos_block *block,int flag);
extern void scicos_hystheresis_block(scicos_block *block,int flag);
extern void scicos_ramp_block(scicos_block *block,int flag);
extern void scicos_minmax_block(scicos_block *block,int flag);
extern void scicos_modulo_count_block(scicos_block *block,int flag);
extern void scicos_mswitch_block(scicos_block *block,int flag);
extern void scicos_product_block(scicos_block *block,int flag);
extern void scicos_ratelimiter_block(scicos_block *block,int flag);
extern void scicos_integral_func_block(scicos_block *block,int flag);
extern void scicos_evtvardly_block(scicos_block *block,int flag);
extern void scicos_relationalop_block(scicos_block *block,int flag);
extern void scicos_bounce_ball_block(scicos_block *block,int flag);
extern void scicos_bouncexy_block(scicos_block *block,int flag);
extern void scicos_cscope_block(scicos_block *block,int flag);
extern void scicos_cmscope_block(scicos_block *block,int flag);
extern void scicos_scalar2vector_block(scicos_block *block,int flag);
extern void scicos_evaluate_expr_block(scicos_block *block,int flag);
extern void scicos_cstblk4_block(scicos_block *block,int flag);
extern void scicos_transmit_or_zero_block(scicos_block *block,int flag);
extern void scicos_mvswitch_block(scicos_block *block,int flag);
extern void scicos_csslti4_block(scicos_block *block,int flag);

#define SCICOS44 

#ifdef SCICOS44 

extern void selector (scicos_block *block,int flag);
extern void sum (scicos_block *block,int flag);
extern void prod (scicos_block *block,int flag);
extern void switchn (scicos_block *block,int flag);
extern void relay (scicos_block *block,int flag);
extern void readc (scicos_block *block,int flag);
extern void writec (scicos_block *block,int flag);
extern void writeau (scicos_block *block,int flag);
extern void readau (scicos_block *block,int flag);
extern void plusblk (scicos_block *block,int flag);
extern void slider (scicos_block *block,int flag);
extern void zcross2 (scicos_block *block,int flag);
extern void mswitch (scicos_block *block,int flag);
extern void logicalop (scicos_block *block,int flag);
extern void switch2 (scicos_block *block,int flag);
extern void variable_delay (scicos_block *block,int flag);
extern void time_delay (scicos_block *block,int flag);
extern void cscope (scicos_block *block,int flag);
extern void cmscope (scicos_block *block,int flag);
extern void satur (scicos_block *block,int flag);
extern void step_func (scicos_block *block,int flag);
extern void integral_func (scicos_block *block,int flag);
extern void absolute_value (scicos_block *block,int flag);
extern void bounce_ball (scicos_block *block,int flag);
extern void bouncexy (scicos_block *block,int flag);
extern void extractor (scicos_block *block,int flag);
extern void scalar2vector (scicos_block *block,int flag);
extern void minmax (scicos_block *block,int flag);
extern void signum (scicos_block *block,int flag);
extern void product (scicos_block *block,int flag);
extern void summation (scicos_block *block,int flag);
extern void multiplex (scicos_block *block,int flag);
extern void gainblk (scicos_block *block,int flag);
extern void relationalop (scicos_block *block,int flag);
extern void modulo_count (scicos_block *block,int flag);
extern void hystheresis (scicos_block *block,int flag);
extern void ratelimiter (scicos_block *block,int flag);
extern void backlash (scicos_block *block,int flag);
extern void deadband (scicos_block *block,int flag);
extern void ramp (scicos_block *block,int flag);
extern void evaluate_expr (scicos_block *block,int flag);
extern void deriv (scicos_block *block,int flag);
extern void sin_blk (scicos_block *block,int flag);
extern void cos_blk (scicos_block *block,int flag);
extern void tan_blk (scicos_block *block,int flag);
extern void asin_blk (scicos_block *block,int flag);
extern void acos_blk (scicos_block *block,int flag);
extern void atan_blk (scicos_block *block,int flag);
extern void sinh_blk (scicos_block *block,int flag);
extern void cosh_blk (scicos_block *block,int flag);
extern void tanh_blk (scicos_block *block,int flag);
extern void asinh_blk (scicos_block *block,int flag);
extern void acosh_blk (scicos_block *block,int flag);
extern void atanh_blk (scicos_block *block,int flag);
extern void evtvardly (scicos_block *block,int flag);
extern void edgetrig (scicos_block *block,int flag);
extern void tcslti4 (scicos_block *block,int flag);
extern void tcsltj4 (scicos_block *block,int flag);
extern void dsslti4 (scicos_block *block,int flag);
extern void csslti4 (scicos_block *block,int flag);
extern void cstblk4 (scicos_block *block,int flag);
extern void samphold4 (scicos_block *block,int flag);
extern void dollar4 (scicos_block *block,int flag);
extern void invblk4 (scicos_block *block,int flag);
extern void delay4 (scicos_block *block,int flag);
extern void cevscpe (scicos_block *block,int flag);
extern void cfscope (scicos_block *block,int flag);
extern void cscopxy (scicos_block *block,int flag);
extern void canimxy (scicos_block *block,int flag);
extern void canimxy3d (scicos_block *block,int flag);
extern void cscopxy3d (scicos_block *block,int flag);
extern void matmul_m (scicos_block *block,int flag);
extern void mattran_m (scicos_block *block,int flag);
extern void cmatview (scicos_block *block,int flag);
extern void cmat3d (scicos_block *block,int flag);
extern void extdiag (scicos_block *block,int flag);
extern void exttril (scicos_block *block,int flag);
extern void mat_bksl (scicos_block *block,int flag);
extern void mat_diag (scicos_block *block,int flag);
extern void mat_lu (scicos_block *block,int flag);
extern void mat_svd (scicos_block *block,int flag);
extern void matz_absc (scicos_block *block,int flag);
extern void matz_conj (scicos_block *block,int flag);
extern void matz_expm (scicos_block *block,int flag);
extern void matz_reim (scicos_block *block,int flag);
extern void matz_svd (scicos_block *block,int flag);
extern void root_coef (scicos_block *block,int flag);
extern void extdiagz (scicos_block *block,int flag);
extern void exttrilz (scicos_block *block,int flag);
extern void mat_cath (scicos_block *block,int flag);
extern void mat_div (scicos_block *block,int flag);
extern void mat_pinv (scicos_block *block,int flag);
extern void mat_vps (scicos_block *block,int flag);
extern void matz_bksl (scicos_block *block,int flag);
extern void matz_det (scicos_block *block,int flag);
extern void matz_inv (scicos_block *block,int flag);
extern void matz_reimc (scicos_block *block,int flag);
extern void matz_vps (scicos_block *block,int flag);
extern void rootz_coef (scicos_block *block,int flag);
extern void extract (scicos_block *block,int flag);
extern void exttriu (scicos_block *block,int flag);
extern void mat_catv (scicos_block *block,int flag);
extern void mat_expm (scicos_block *block,int flag);
extern void mat_reshape (scicos_block *block,int flag);
extern void mat_vpv (scicos_block *block,int flag);
extern void matz_cath (scicos_block *block,int flag);
extern void matz_diag (scicos_block *block,int flag);
extern void matz_lu (scicos_block *block,int flag);
extern void matz_reshape (scicos_block *block,int flag);
extern void matz_vpv (scicos_block *block,int flag);
extern void submat (scicos_block *block,int flag);
extern void extractz (scicos_block *block,int flag);
extern void exttriuz (scicos_block *block,int flag);
extern void mat_det (scicos_block *block,int flag);
extern void mat_inv (scicos_block *block,int flag);
extern void mat_sing (scicos_block *block,int flag);
extern void matz_abs (scicos_block *block,int flag);
extern void matz_catv (scicos_block *block,int flag);
extern void matz_div (scicos_block *block,int flag);
extern void matz_pinv (scicos_block *block,int flag);
extern void matz_sing (scicos_block *block,int flag);
extern void ricc_m (scicos_block *block,int flag);
extern void submatz (scicos_block *block,int flag);
extern void switch2_m (scicos_block *block,int flag);
extern void dollar4_m (scicos_block *block,int flag);
extern void cstblk4_m (scicos_block *block,int flag);
extern void integralz_func (scicos_block *block,int flag);
extern void matzmul_m (scicos_block *block,int flag);
extern void matztran_m (scicos_block *block,int flag);
extern void mat_sum (scicos_block *block,int flag);
extern void mat_sumc (scicos_block *block,int flag);
extern void mat_suml (scicos_block *block,int flag);
extern void cumsum_c (scicos_block *block,int flag);
extern void cumsum_m (scicos_block *block,int flag);
extern void cumsum_r (scicos_block *block,int flag);
extern void matz_sum (scicos_block *block,int flag);
extern void matz_sumc (scicos_block *block,int flag);
extern void matz_suml (scicos_block *block,int flag);
extern void cumsumz_c (scicos_block *block,int flag);
extern void cumsumz_m (scicos_block *block,int flag);
extern void cumsumz_r (scicos_block *block,int flag);
extern void selector_m (scicos_block *block,int flag);
extern void summation_z (scicos_block *block,int flag);
extern void convert (scicos_block *block,int flag);
extern void logicalop_i32 (scicos_block *block,int flag);
extern void logicalop_ui32 (scicos_block *block,int flag);
extern void logicalop_i16 (scicos_block *block,int flag);
extern void logicalop_ui16 (scicos_block *block,int flag);
extern void logicalop_i8 (scicos_block *block,int flag);
extern void logicalop_ui8 (scicos_block *block,int flag);
extern void logicalop_m (scicos_block *block,int flag);
extern void samphold4_m (scicos_block *block,int flag);
extern void matmul_i32s (scicos_block *block,int flag);
extern void matmul_i32n (scicos_block *block,int flag);
extern void matmul_i32e (scicos_block *block,int flag);
extern void matmul_i16s (scicos_block *block,int flag);
extern void matmul_i16n (scicos_block *block,int flag);
extern void matmul_i16e (scicos_block *block,int flag);
extern void matmul_i8s (scicos_block *block,int flag);
extern void matmul_i8n (scicos_block *block,int flag);
extern void matmul_i8e (scicos_block *block,int flag);
extern void matmul_ui32s (scicos_block *block,int flag);
extern void matmul_ui32n (scicos_block *block,int flag);
extern void matmul_ui32e (scicos_block *block,int flag);
extern void matmul_ui16s (scicos_block *block,int flag);
extern void matmul_ui16n (scicos_block *block,int flag);
extern void matmul_ui16e (scicos_block *block,int flag);
extern void matmul_ui8s (scicos_block *block,int flag);
extern void matmul_ui8n (scicos_block *block,int flag);
extern void matmul_ui8e (scicos_block *block,int flag);
extern void summation_i32s (scicos_block *block,int flag);
extern void summation_i32n (scicos_block *block,int flag);
extern void summation_i32e (scicos_block *block,int flag);
extern void summation_i16s (scicos_block *block,int flag);
extern void summation_i16n (scicos_block *block,int flag);
extern void summation_i16e (scicos_block *block,int flag);
extern void summation_i8s (scicos_block *block,int flag);
extern void summation_i8n (scicos_block *block,int flag);
extern void summation_i8e (scicos_block *block,int flag);
extern void summation_ui32s (scicos_block *block,int flag);
extern void summation_ui32n (scicos_block *block,int flag);
extern void summation_ui32e (scicos_block *block,int flag);
extern void summation_ui16s (scicos_block *block,int flag);
extern void summation_ui16n (scicos_block *block,int flag);
extern void summation_ui16e (scicos_block *block,int flag);
extern void summation_ui8s (scicos_block *block,int flag);
extern void summation_ui8n (scicos_block *block,int flag);
extern void summation_ui8e (scicos_block *block,int flag);
extern void gainblk_i32s (scicos_block *block,int flag);
extern void gainblk_i32n (scicos_block *block,int flag);
extern void gainblk_i32e (scicos_block *block,int flag);
extern void gainblk_i16s (scicos_block *block,int flag);
extern void gainblk_i16n (scicos_block *block,int flag);
extern void gainblk_i16e (scicos_block *block,int flag);
extern void gainblk_i8s (scicos_block *block,int flag);
extern void gainblk_i8n (scicos_block *block,int flag);
extern void gainblk_i8e (scicos_block *block,int flag);
extern void gainblk_ui32s (scicos_block *block,int flag);
extern void gainblk_ui32n (scicos_block *block,int flag);
extern void gainblk_ui32e (scicos_block *block,int flag);
extern void gainblk_ui16s (scicos_block *block,int flag);
extern void gainblk_ui16n (scicos_block *block,int flag);
extern void gainblk_ui16e (scicos_block *block,int flag);
extern void gainblk_ui8s (scicos_block *block,int flag);
extern void gainblk_ui8n (scicos_block *block,int flag);
extern void gainblk_ui8e (scicos_block *block,int flag);
extern void delay4_i32 (scicos_block *block,int flag);
extern void delay4_i16 (scicos_block *block,int flag);
extern void delay4_i8 (scicos_block *block,int flag);
extern void delay4_ui32 (scicos_block *block,int flag);
extern void delay4_ui16 (scicos_block *block,int flag);
extern void delay4_ui8 (scicos_block *block,int flag);
extern void mat_sqrt (scicos_block *block,int flag);
extern void matz_sqrt (scicos_block *block,int flag);
extern void relational_op_i32 (scicos_block *block,int flag);
extern void relational_op_ui32 (scicos_block *block,int flag);
extern void relational_op_i16 (scicos_block *block,int flag);
extern void relational_op_ui16 (scicos_block *block,int flag);
extern void relational_op_i8 (scicos_block *block,int flag);
extern void relational_op_ui8 (scicos_block *block,int flag);
extern void evtdly4 (scicos_block *block,int flag);
extern void matmul2_m (scicos_block *block,int flag);
extern void matzmul2_m (scicos_block *block,int flag);
extern void expblk_m (scicos_block *block,int flag);
extern void logic (scicos_block *block,int flag);
extern void bit_clear_32 (scicos_block *block,int flag);
extern void bit_clear_16 (scicos_block *block,int flag);
extern void bit_clear_8 (scicos_block *block,int flag);
extern void bit_set_32 (scicos_block *block,int flag);
extern void bit_set_16 (scicos_block *block,int flag);
extern void bit_set_8 (scicos_block *block,int flag);
extern void extract_bit_32_UH0 (scicos_block *block,int flag);
extern void extract_bit_16_UH0 (scicos_block *block,int flag);
extern void extract_bit_8_UH0 (scicos_block *block,int flag);
extern void extract_bit_32_UH1 (scicos_block *block,int flag);
extern void extract_bit_16_UH1 (scicos_block *block,int flag);
extern void extract_bit_8_UH1 (scicos_block *block,int flag);
extern void extract_bit_32_LH (scicos_block *block,int flag);
extern void extract_bit_16_LH (scicos_block *block,int flag);
extern void extract_bit_8_LH (scicos_block *block,int flag);
extern void extract_bit_32_MSB0 (scicos_block *block,int flag);
extern void extract_bit_16_MSB0 (scicos_block *block,int flag);
extern void extract_bit_8_MSB0 (scicos_block *block,int flag);
extern void extract_bit_32_MSB1 (scicos_block *block,int flag);
extern void extract_bit_16_MSB1 (scicos_block *block,int flag);
extern void extract_bit_8_MSB1 (scicos_block *block,int flag);
extern void extract_bit_32_LSB (scicos_block *block,int flag);
extern void extract_bit_16_LSB (scicos_block *block,int flag);
extern void extract_bit_8_LSB (scicos_block *block,int flag);
extern void extract_bit_32_RB0 (scicos_block *block,int flag);
extern void extract_bit_16_RB0 (scicos_block *block,int flag);
extern void extract_bit_8_RB0 (scicos_block *block,int flag);
extern void extract_bit_32_RB1 (scicos_block *block,int flag);
extern void extract_bit_16_RB1 (scicos_block *block,int flag);
extern void extract_bit_8_RB1 (scicos_block *block,int flag);
extern void shift_8_LA (scicos_block *block,int flag);
extern void shift_16_LA (scicos_block *block,int flag);
extern void shift_32_LA (scicos_block *block,int flag);
extern void shift_8_LC (scicos_block *block,int flag);
extern void shift_16_LC (scicos_block *block,int flag);
extern void shift_32_LC (scicos_block *block,int flag);
extern void shift_8_RA (scicos_block *block,int flag);
extern void shift_16_RA (scicos_block *block,int flag);
extern void shift_32_RA (scicos_block *block,int flag);
extern void shift_8_RC (scicos_block *block,int flag);
extern void shift_16_RC (scicos_block *block,int flag);
extern void shift_32_RC (scicos_block *block,int flag);
extern void shift_u8_RA (scicos_block *block,int flag);
extern void shift_u16_RA (scicos_block *block,int flag);
extern void shift_u32_RA (scicos_block *block,int flag);
extern void extract_bit_u32_UH1 (scicos_block *block,int flag);
extern void extract_bit_u16_UH1 (scicos_block *block,int flag);
extern void extract_bit_u8_UH1 (scicos_block *block,int flag);
extern void extract_bit_u32_MSB1 (scicos_block *block,int flag);
extern void extract_bit_u16_MSB1 (scicos_block *block,int flag);
extern void extract_bit_u8_MSB1 (scicos_block *block,int flag);
extern void extract_bit_u32_RB1 (scicos_block *block,int flag);
extern void extract_bit_u16_RB1 (scicos_block *block,int flag);
extern void extract_bit_u8_RB1 (scicos_block *block,int flag);
extern void rndblk_m (scicos_block *block,int flag);
extern void relational_op (scicos_block *block,int flag);
extern void curve_c (scicos_block *block,int flag);
extern void counter (scicos_block *block,int flag);
extern void m_frequ (scicos_block *block,int flag);
extern void tows_c (scicos_block *block,int flag);
extern void rndblkz_m (scicos_block *block,int flag);
extern void fromws_c (scicos_block *block,int flag);
extern void mathermit_m (scicos_block *block,int flag);
extern void scicosexit (scicos_block *block,int flag);
extern void automat (scicos_block *block,int flag);
extern void lookup_c (scicos_block *block,int flag);
extern void tablex2d_c (scicos_block *block,int flag);
extern void matbyscal (scicos_block *block,int flag);
extern void matbyscal_s (scicos_block *block,int flag);
extern void matbyscal_e (scicos_block *block,int flag);
extern void matmul2_s (scicos_block *block,int flag);
extern void matmul2_e (scicos_block *block,int flag);
extern void constraint_c (scicos_block *block,int flag);
extern void lookup2d (scicos_block *block,int flag);
extern void diffblk_c (scicos_block *block,int flag);
extern void andlog (scicos_block *block,int flag);
extern void foriterator (scicos_block *block,int flag);
extern void assignment (scicos_block *block,int flag);
extern void whileiterator (scicos_block *block,int flag);
extern void loopbreaker   (scicos_block *block,int flag) ;
#endif 

#endif 

/* this is to be included only once in scicos.c */

#ifdef TABSIM 

#ifndef SCICOS44 
scicos_block_table  tabsim[] ={
  {"absblk",(ScicosF) scicos_abs_block},
  {"absolute_value",(ScicosF) scicos_absolute_value_block},
  {"acos_blk",(ScicosF) scicos_acos_block},
  {"acosh_blk",(ScicosF) scicos_acosh_block},
  {"affich",(ScicosF) scicos_affich_block},
  {"andlog",(ScicosF) scicos_andlog_block},
  {"asin_blk",(ScicosF) scicos_asin_block},
  {"asinh_blk",(ScicosF) scicos_asinh_block},
  {"atan_blk",(ScicosF) scicos_atan_block},
  {"atanh_blk",(ScicosF) scicos_atanh_block},
  {"backlash",(ScicosF) scicos_backlash_block},
  {"bidon",(ScicosF) scicos_bidon_block},
  {"bounce_ball",(ScicosF) scicos_bounce_ball_block},
  {"bouncexy",(ScicosF) scicos_bouncexy_block},
  {"cdummy",(ScicosF) scicos_cdummy_block},
  {"cmscope",(ScicosF) scicos_cmscope_block},
  {"constraint",(ScicosF) scicos_constraint_block},
  {"cos_blk",(ScicosF) scicos_cos_block},
  {"cosblk",(ScicosF) scicos_cosblk_block},
  {"cosh_blk",(ScicosF) scicos_cosh_block},
  {"cscope",(ScicosF) scicos_cscope_block},
  {"csslti",(ScicosF) scicos_csslti_block},
  {"csslti4",(ScicosF) scicos_csslti4_block},
  {"cstblk",(ScicosF) scicos_cst_block},
  {"cstblk4",(ScicosF) scicos_cstblk4_block},
  {"dband",(ScicosF) scicos_dband_block},
  {"deadband",(ScicosF) scicos_deadband_block},
  {"delay",(ScicosF) scicos_delay_block},
  {"delayv",(ScicosF) scicos_delayv_block},
  {"demux",(ScicosF) scicos_demux_block},
  {"deriv",(ScicosF) scicos_deriv_block},
  {"diffblk",(ScicosF) scicos_diff_block},
  {"dlradp",(ScicosF) scicos_dlradp_block},
  {"dollar",(ScicosF) scicos_dollar_block},
  {"dsslti",(ScicosF) scicos_dsslti_block},
  {"eselect",(ScicosF) scicos_eselect_block},
  {"evaluate_expr",(ScicosF) scicos_evaluate_expr_block},
  {"evscpe",(ScicosF) scicos_evscpe_block},
  {"evtdly",(ScicosF) scicos_evtdly_block},
  {"evtvardly",(ScicosF) scicos_evtvardly_block},
  {"expblk",(ScicosF) scicos_exp_block},
  {"extractor",(ScicosF) scicos_extractor_block},
  {"forblk",(ScicosF) scicos_for_block},
  {"fscope",(ScicosF) scicos_fscope_block},
  {"fsv",(ScicosF) scicos_fsv_block},
  {"gainblk",(ScicosF) scicos_gainblk_block},
  {"gain",(ScicosF) scicos_gain_block},
  {"gensin",(ScicosF) scicos_gensin_block},
  {"gensqr",(ScicosF) scicos_gensqr_block},
  {"hltblk",(ScicosF) scicos_hlt_block},
  {"hystheresis",(ScicosF) scicos_hystheresis_block},
  {"ifthel",(ScicosF) scicos_ifthel_block},
  {"integral_func",(ScicosF) scicos_integral_func_block},
  {"integr",(ScicosF) scicos_integr_block},
  {"intplt",(ScicosF) scicos_intplt_block},
  {"intpol",(ScicosF) scicos_intpol_block},
  {"intrp2",(ScicosF) scicos_intrp2_block},
  {"intrpl",(ScicosF) scicos_intrpl_block},
  {"invblk",(ScicosF) scicos_inv_block},
  {"iocopy",(ScicosF) scicos_iocopy_block},
  {"logblk",(ScicosF) scicos_log_block},
  {"logicalop",(ScicosF) scicos_logicalop_block},
  {"lookup",(ScicosF) scicos_lookup_block},
  {"lsplit",(ScicosF) scicos_lsplit_block},
  {"lusat",(ScicosF) scicos_lusat_block},
  {"maxblk",(ScicosF) scicos_max_block},
  {"memo",(ScicosF) scicos_memo_block},
  {"mfclck",(ScicosF) scicos_mfclck_block},
  {"minblk",(ScicosF) scicos_min_block},
  {"minmax",(ScicosF) scicos_minmax_block},
  {"modulo_count",(ScicosF) scicos_modulo_count_block},
  {"mscope",(ScicosF) scicos_mscope_block},
  {"mswitch",(ScicosF) scicos_mswitch_block},
  {"multiplex",(ScicosF) scicos_multiplex_block},
  {"mux",(ScicosF) scicos_mux_block},
  {"mvswitch",(ScicosF) scicos_mvswitch_block},
  {"pload",(ScicosF) scicos_pload_block},
  {"plusblk",(ScicosF) scicos_plus_block},
  {"powblk",(ScicosF) scicos_pow_block},
  {"prod",(ScicosF) scicos_prod_block},
  {"product",(ScicosF) scicos_product_block},
  {"qzcel",(ScicosF) scicos_qzcel_block},
  {"qzflr",(ScicosF) scicos_qzflr_block},
  {"qzrnd",(ScicosF) scicos_qzrnd_block},
  {"qztrn",(ScicosF) scicos_qztrn_block},
  {"ramp",(ScicosF) scicos_ramp_block},
  {"ratelimiter",(ScicosF) scicos_ratelimiter_block},
  {"readau",(ScicosF) scicos_readau_block},
  {"readc",(ScicosF) scicos_readc_block},
  {"readf",(ScicosF) scicos_readf_block},
  {"relationalop",(ScicosF) scicos_relationalop_block},
  {"relay",(ScicosF) scicos_relay_block},
  {"rndblk",(ScicosF) scicos_rnd_block},
  {"samphold",(ScicosF) scicos_samphold_block},
  {"satur",(ScicosF) scicos_satur_block},
  {"sawtth",(ScicosF) scicos_sawtth_block},
  {"scalar2vector",(ScicosF) scicos_scalar2vector_block},
  {"scope",(ScicosF) scicos_scope_block},
  {"scopxy",(ScicosF) scicos_scopxy_block},
  {"scoxy",(ScicosF) scicos_scoxy_block},
  {"selblk",(ScicosF) scicos_sel_block},
  {"selector",(ScicosF) scicos_selector_block},
  {"signum",(ScicosF) scicos_signum_block},
  {"sinblk",(ScicosF) scicos_sinblk_block},
  {"sin_blk",(ScicosF) scicos_sin_block},
  {"sinh_blk",(ScicosF) scicos_sinh_block},
  {"slider",(ScicosF) scicos_slider_block},
  {"sqrblk",(ScicosF) scicos_sqr_block},
  {"step_func",(ScicosF) scicos_step_func_block},
  {"sum2",(ScicosF) scicos_sum2_block},
  {"sum3",(ScicosF) scicos_sum3_block},
  {"summation",(ScicosF) scicos_summation_block},
  {"sum",(ScicosF) scicos_sum_block},
  {"switch2",(ScicosF) scicos_switch2_block},
  {"switchn",(ScicosF) scicos_switchn_block},
  {"tanblk",(ScicosF) scicos_tanblk_block},
  {"tan_blk",(ScicosF) scicos_tan_block},
  {"tanh_blk",(ScicosF) scicos_tanh_block},
  {"tcslti",(ScicosF) scicos_tcslti_block},
  {"tcsltj",(ScicosF) scicos_tcsltj_block},
  {"timblk",(ScicosF) scicos_timblk_block},
  {"time_delay",(ScicosF) scicos_time_delay_block},
  {"transmit_or_zero",(ScicosF) scicos_transmit_or_zero_block},
  {"trash",(ScicosF) scicos_trash_block},
  {"variable_delay",(ScicosF) scicos_variable_delay_block},
  {"writeau",(ScicosF) scicos_writeau_block},
  {"writec",(ScicosF) scicos_writec_block},
  {"writef",(ScicosF) scicos_writef_block},
  {"zcross2",(ScicosF) scicos_zcross2_block},  
  {"zcross",(ScicosF) scicos_zcross_block} ,
  {NULL , (ScicosF) 0}
};

#else 

scicos_block_table  tabsim[] ={
  {"absblk",(ScicosF) scicos_abs_block},
  {"absolute_value",(ScicosF) scicos_absolute_value_block},
  /* {"acos_blk",(ScicosF) acos_blk}, */
  {"acos_blk",(ScicosF) scicos_acos_block},
  /* {"acosh_blk",(ScicosF) acosh_blk}, */
  {"acosh_blk",(ScicosF) scicos_acosh_block},
  /*  {"affich",(ScicosF) F2C(affich)}, */
  {"affich",(ScicosF) scicos_affich_block},
  {"affich2", (ScicosF)  scicos_affich2_block },
  /*  {"affich2",(ScicosF) F2C(affich2)}, */
  /* {"andlog",(ScicosF) andlog}, */
  {"andlog",(ScicosF) scicos_andlog_block},
  /* {"asin_blk",(ScicosF) asin_blk}, */
  {"asin_blk",(ScicosF) scicos_asin_block},
  /* {"asinh_blk",(ScicosF) asinh_blk}, */
  {"asinh_blk",(ScicosF) scicos_asinh_block},
  {"assignment",(ScicosF) assignment},
  /* {"atan_blk",(ScicosF) atan_blk}, */
  {"atan_blk",(ScicosF) scicos_atan_block},
  /* {"atanh_blk",(ScicosF) atanh_blk}, */
  {"atanh_blk",(ScicosF) scicos_atanh_block},
  {"automat",(ScicosF) automat},
  /* {"backlash",(ScicosF) backlash}, */
  {"backlash",(ScicosF) scicos_backlash_block},
  /*  {"bidon",(ScicosF) F2C(bidon)}, */
  {"bidon",(ScicosF) scicos_bidon_block},
  {"bit_clear_16",(ScicosF) bit_clear_16},
  {"bit_clear_32",(ScicosF) bit_clear_32},
  {"bit_clear_8",(ScicosF) bit_clear_8},
  {"bit_set_16",(ScicosF) bit_set_16},
  {"bit_set_32",(ScicosF) bit_set_32},
  {"bit_set_8",(ScicosF) bit_set_8},
  /* {"bounce_ball",(ScicosF) bounce_ball}, */
  {"bounce_ball",(ScicosF) scicos_bounce_ball_block},
  /* {"bouncexy",(ScicosF) bouncexy}, */
  {"bouncexy",(ScicosF) scicos_bouncexy_block},
  {"canimxy",(ScicosF) canimxy},
  {"canimxy3d",(ScicosF) canimxy3d},
  /*  {"cdummy",(ScicosF) F2C(cdummy)}, */
  {"cdummy",(ScicosF) scicos_cdummy_block},
  {"cevscpe",(ScicosF) cevscpe},
  /* {"cfscope",(ScicosF) cfscope}, XXX */
  {"cmat3d",(ScicosF) cmat3d},
  {"cmatview",(ScicosF) cmatview},
  /* {"cmscope",(ScicosF) cmscope}, */
  {"cmscope",(ScicosF) scicos_cmscope_block},
  {"constraint",(ScicosF) scicos_constraint_block},
  {"constraint_c",(ScicosF) constraint_c}, 
  {"convert",(ScicosF) convert},
  /* {"cos_blk",(ScicosF) cos_blk}, */
  {"cos_blk",(ScicosF) scicos_cos_block},
  /*  {"cosblk",(ScicosF) F2C(cosblk)}, */
  {"cosblk",(ScicosF) scicos_cosblk_block},
  /* {"cosh_blk",(ScicosF) cosh_blk}, */
  {"cosh_blk",(ScicosF) scicos_cosh_block},
  {"counter",(ScicosF) counter},
  /* {"cscope",(ScicosF) cscope}, */
  {"cscope",(ScicosF) scicos_cscope_block},
  {"cscopxy",(ScicosF) cscopxy},
  {"cscopxy3d",(ScicosF) cscopxy3d},
  /*  {"csslti",(ScicosF) F2C(csslti)}, */
  {"csslti",(ScicosF) scicos_csslti_block},
  /* {"csslti4",(ScicosF) csslti4}, */
  {"csslti4",(ScicosF) scicos_csslti4_block},
  /*  {"cstblk",(ScicosF) F2C(cstblk)}, */
  {"cstblk",(ScicosF) scicos_cst_block},
  /* {"cstblk4",(ScicosF) cstblk4}, */
  {"cstblk4",(ScicosF) scicos_cstblk4_block},
  {"cstblk4_m",(ScicosF) cstblk4_m},
  {"cumsum_c",(ScicosF) cumsum_c},
  {"cumsum_m",(ScicosF) cumsum_m},
  {"cumsum_r",(ScicosF) cumsum_r},
  {"cumsumz_c",(ScicosF) cumsumz_c},
  {"cumsumz_m",(ScicosF) cumsumz_m},
  {"cumsumz_r",(ScicosF) cumsumz_r},
  {"curve_c",(ScicosF) curve_c},
  /*  {"dband",(ScicosF) F2C(dband)}, */
  {"dband",(ScicosF) scicos_dband_block},
  /* {"deadband",(ScicosF) deadband}, */
  {"deadband",(ScicosF) scicos_deadband_block},
  /*  {"delay",(ScicosF) F2C(delay)}, */
  {"delay",(ScicosF) scicos_delay_block},
  {"delay4",(ScicosF) delay4},
  {"delay4_i16",(ScicosF) delay4_i16},
  {"delay4_i32",(ScicosF) delay4_i32},
  {"delay4_i8",(ScicosF) delay4_i8},
  {"delay4_ui16",(ScicosF) delay4_ui16},
  {"delay4_ui32",(ScicosF) delay4_ui32},
  {"delay4_ui8",(ScicosF) delay4_ui8},
  /*  {"delayv",(ScicosF) F2C(delayv)}, */
  {"delayv",(ScicosF) scicos_delayv_block},
  /*  {"demux",(ScicosF) F2C(demux)}, */
  {"demux",(ScicosF) scicos_demux_block},
  /* {"deriv",(ScicosF) deriv}, */
  {"deriv",(ScicosF) scicos_deriv_block},
  {"diffblk",(ScicosF) scicos_diff_block},
  {"diffblk_c",(ScicosF) diffblk_c},
  /*  {"dlradp",(ScicosF) F2C(dlradp)}, */
  {"dlradp",(ScicosF) scicos_dlradp_block},
  /*  {"dollar",(ScicosF) F2C(dollar)}, */
  {"dollar",(ScicosF) scicos_dollar_block},
  {"dollar4",(ScicosF) dollar4},
  {"dollar4_m",(ScicosF) dollar4_m},
  /*  {"dsslti",(ScicosF) F2C(dsslti)}, */
  {"dsslti",(ScicosF) scicos_dsslti_block},
  {"dsslti4",(ScicosF) dsslti4},
  {"edgetrig",(ScicosF) edgetrig},
  /*  {"eselect",(ScicosF) F2C(eselect)}, */
  {"eselect",(ScicosF) scicos_eselect_block},
  /* {"evaluate_expr",(ScicosF) evaluate_expr}, */
  {"evaluate_expr",(ScicosF) scicos_evaluate_expr_block},
  {"evscpe",(ScicosF) scicos_evscpe_block},
  /*  {"evtdly",(ScicosF) F2C(evtdly)}, */
  {"evtdly",(ScicosF) scicos_evtdly_block},
  {"evtdly4",(ScicosF) evtdly4},
  /* {"evtvardly",(ScicosF) evtvardly}, */
  {"evtvardly",(ScicosF) scicos_evtvardly_block},
  /*  {"expblk",(ScicosF) F2C(expblk)}, */
  {"expblk",(ScicosF) scicos_exp_block},
  {"expblk_m",(ScicosF) expblk_m},
  {"extdiag",(ScicosF) extdiag},
  {"extdiagz",(ScicosF) extdiagz},
  {"extract",(ScicosF) extract},
  {"extract_bit_16_LH",(ScicosF) extract_bit_16_LH},
  {"extract_bit_16_LSB",(ScicosF) extract_bit_16_LSB},
  {"extract_bit_16_MSB0",(ScicosF) extract_bit_16_MSB0},
  {"extract_bit_16_MSB1",(ScicosF) extract_bit_16_MSB1},
  {"extract_bit_16_RB0",(ScicosF) extract_bit_16_RB0},
  {"extract_bit_16_RB1",(ScicosF) extract_bit_16_RB1},
  {"extract_bit_16_UH0",(ScicosF) extract_bit_16_UH0},
  {"extract_bit_16_UH1",(ScicosF) extract_bit_16_UH1},
  {"extract_bit_32_LH",(ScicosF) extract_bit_32_LH},
  {"extract_bit_32_LSB",(ScicosF) extract_bit_32_LSB},
  {"extract_bit_32_MSB0",(ScicosF) extract_bit_32_MSB0},
  {"extract_bit_32_MSB1",(ScicosF) extract_bit_32_MSB1},
  {"extract_bit_32_RB0",(ScicosF) extract_bit_32_RB0},
  {"extract_bit_32_RB1",(ScicosF) extract_bit_32_RB1},
  {"extract_bit_32_UH0",(ScicosF) extract_bit_32_UH0},
  {"extract_bit_32_UH1",(ScicosF) extract_bit_32_UH1},
  {"extract_bit_8_LH",(ScicosF) extract_bit_8_LH},
  {"extract_bit_8_LSB",(ScicosF) extract_bit_8_LSB},
  {"extract_bit_8_MSB0",(ScicosF) extract_bit_8_MSB0},
  {"extract_bit_8_MSB1",(ScicosF) extract_bit_8_MSB1},
  {"extract_bit_8_RB0",(ScicosF) extract_bit_8_RB0},
  {"extract_bit_8_RB1",(ScicosF) extract_bit_8_RB1},
  {"extract_bit_8_UH0",(ScicosF) extract_bit_8_UH0},
  {"extract_bit_8_UH1",(ScicosF) extract_bit_8_UH1},
  {"extract_bit_u16_MSB1",(ScicosF) extract_bit_u16_MSB1},
  {"extract_bit_u16_RB1",(ScicosF) extract_bit_u16_RB1},
  {"extract_bit_u16_UH1",(ScicosF) extract_bit_u16_UH1},
  {"extract_bit_u32_MSB1",(ScicosF) extract_bit_u32_MSB1},
  {"extract_bit_u32_RB1",(ScicosF) extract_bit_u32_RB1},
  {"extract_bit_u32_UH1",(ScicosF) extract_bit_u32_UH1},
  {"extract_bit_u8_MSB1",(ScicosF) extract_bit_u8_MSB1},
  {"extract_bit_u8_RB1",(ScicosF) extract_bit_u8_RB1},
  {"extract_bit_u8_UH1",(ScicosF) extract_bit_u8_UH1},
  /* {"extractor",(ScicosF) extractor}, */
  {"extractor",(ScicosF) scicos_extractor_block},
  {"extractz",(ScicosF) extractz},
  {"exttril",(ScicosF) exttril},
  {"exttrilz",(ScicosF) exttrilz},
  {"exttriu",(ScicosF) exttriu},
  {"exttriuz",(ScicosF) exttriuz},
  /*  {"forblk",(ScicosF) F2C(forblk)}, */
  {"forblk",(ScicosF) scicos_for_block},
  {"foriterator",(ScicosF) foriterator},
  {"fromws_c",(ScicosF) fromws_c},
  {"fscope",(ScicosF) scicos_fscope_block},
  /*  {"fsv",(ScicosF) F2C(fsv)}, */
  {"fsv",(ScicosF) scicos_fsv_block},
  /*  {"gain",(ScicosF) F2C(gain)}, */
  {"gain",(ScicosF) scicos_gain_block},
  /* {"gainblk",(ScicosF) gainblk}, */
  {"gainblk",(ScicosF) scicos_gainblk_block},
  {"gainblk_i16e",(ScicosF) gainblk_i16e},
  {"gainblk_i16n",(ScicosF) gainblk_i16n},
  {"gainblk_i16s",(ScicosF) gainblk_i16s},
  {"gainblk_i32e",(ScicosF) gainblk_i32e},
  {"gainblk_i32n",(ScicosF) gainblk_i32n},
  {"gainblk_i32s",(ScicosF) gainblk_i32s},
  {"gainblk_i8e",(ScicosF) gainblk_i8e},
  {"gainblk_i8n",(ScicosF) gainblk_i8n},
  {"gainblk_i8s",(ScicosF) gainblk_i8s},
  {"gainblk_ui16e",(ScicosF) gainblk_ui16e},
  {"gainblk_ui16n",(ScicosF) gainblk_ui16n},
  {"gainblk_ui16s",(ScicosF) gainblk_ui16s},
  {"gainblk_ui32e",(ScicosF) gainblk_ui32e},
  {"gainblk_ui32n",(ScicosF) gainblk_ui32n},
  {"gainblk_ui32s",(ScicosF) gainblk_ui32s},
  {"gainblk_ui8e",(ScicosF) gainblk_ui8e},
  {"gainblk_ui8n",(ScicosF) gainblk_ui8n},
  {"gainblk_ui8s",(ScicosF) gainblk_ui8s},
  /*  {"gensin",(ScicosF) F2C(gensin)}, */
  {"gensin",(ScicosF) scicos_gensin_block},
  /*  {"gensqr",(ScicosF) F2C(gensqr)}, */
  {"gensqr",(ScicosF) scicos_gensqr_block},
  /*  {"hltblk",(ScicosF) F2C(hltblk)}, */
  {"hltblk",(ScicosF) scicos_hlt_block},
  /* {"hystheresis",(ScicosF) hystheresis}, */
  {"hystheresis",(ScicosF) scicos_hystheresis_block},
  /*  {"ifthel",(ScicosF) F2C(ifthel)}, */
  {"ifthel",(ScicosF) scicos_ifthel_block},
  /*  {"integr",(ScicosF) F2C(integr)}, */
  {"integr",(ScicosF) scicos_integr_block},
  /* {"integral_func",(ScicosF) integral_func}, */
  {"integral_func",(ScicosF) scicos_integral_func_block},
  {"integralz_func",(ScicosF) integralz_func},
  /*  {"intplt",(ScicosF) F2C(intplt)}, */
  {"intplt",(ScicosF) scicos_intplt_block},
  /*  {"intpol",(ScicosF) F2C(intpol)}, */
  {"intpol",(ScicosF) scicos_intpol_block},
  /*  {"intrp2",(ScicosF) F2C(intrp2)}, */
  {"intrp2",(ScicosF) scicos_intrp2_block},
  /*  {"intrpl",(ScicosF) F2C(intrpl)}, */
  {"intrpl",(ScicosF) scicos_intrpl_block},
  /*  {"invblk",(ScicosF) F2C(invblk)}, */
  {"invblk",(ScicosF) scicos_inv_block},
  {"invblk4",(ScicosF) invblk4},
  /*  {"iocopy",(ScicosF) F2C(iocopy)}, */
  {"iocopy",(ScicosF) scicos_iocopy_block},
  /*  {"logblk",(ScicosF) F2C(logblk)}, */
  {"logblk",(ScicosF) scicos_log_block},
  {"logic",(ScicosF) logic},
  /* {"logicalop",(ScicosF) logicalop}, */
  {"logicalop",(ScicosF) scicos_logicalop_block},
  {"logicalop_i16",(ScicosF) logicalop_i16},
  {"logicalop_i32",(ScicosF) logicalop_i32},
  {"logicalop_i8",(ScicosF) logicalop_i8},
  {"logicalop_m",(ScicosF) logicalop_m},
  {"logicalop_ui16",(ScicosF) logicalop_ui16},
  {"logicalop_ui32",(ScicosF) logicalop_ui32},
  {"logicalop_ui8",(ScicosF) logicalop_ui8},
  /*  {"lookup",(ScicosF) F2C(lookup)}, */
  {"lookup",(ScicosF) scicos_lookup_block},
  {"lookup2d",(ScicosF) lookup2d},
  {"lookup_c",(ScicosF) lookup_c},
  {"loopbreaker",(ScicosF) loopbreaker},
  /*  {"lsplit",(ScicosF) F2C(lsplit)}, */
  {"lsplit",(ScicosF) scicos_lsplit_block},
  /*  {"lusat",(ScicosF) F2C(lusat)}, */
  {"lusat",(ScicosF) scicos_lusat_block},
  {"m_frequ",(ScicosF) m_frequ},
  {"mat_bksl",(ScicosF) mat_bksl},
  {"mat_cath",(ScicosF) mat_cath},
  {"mat_catv",(ScicosF) mat_catv},
  {"mat_det",(ScicosF) mat_det},
  {"mat_diag",(ScicosF) mat_diag},
  {"mat_div",(ScicosF) mat_div},
  {"mat_expm",(ScicosF) mat_expm},
  {"mat_inv",(ScicosF) mat_inv},
  {"mat_lu",(ScicosF) mat_lu},
  {"mat_pinv",(ScicosF) mat_pinv},
  {"mat_reshape",(ScicosF) mat_reshape},
  {"mat_sing",(ScicosF) mat_sing},
  {"mat_sqrt",(ScicosF) mat_sqrt},
  {"mat_sum",(ScicosF) mat_sum},
  {"mat_sumc",(ScicosF) mat_sumc},
  {"mat_suml",(ScicosF) mat_suml},
  {"mat_svd",(ScicosF) mat_svd},
  {"mat_vps",(ScicosF) mat_vps},
  {"mat_vpv",(ScicosF) mat_vpv},
  {"matbyscal",(ScicosF) matbyscal},
  {"matbyscal_e",(ScicosF) matbyscal_e},
  {"matbyscal_s",(ScicosF) matbyscal_s},
  {"mathermit_m",(ScicosF) mathermit_m},
  {"matmul2_e",(ScicosF) matmul2_e},
  {"matmul2_m",(ScicosF) matmul2_m},
  {"matmul2_s",(ScicosF) matmul2_s},
  {"matmul_i16e",(ScicosF) matmul_i16e},
  {"matmul_i16n",(ScicosF) matmul_i16n},
  {"matmul_i16s",(ScicosF) matmul_i16s},
  {"matmul_i32e",(ScicosF) matmul_i32e},
  {"matmul_i32n",(ScicosF) matmul_i32n},
  {"matmul_i32s",(ScicosF) matmul_i32s},
  {"matmul_i8e",(ScicosF) matmul_i8e},
  {"matmul_i8n",(ScicosF) matmul_i8n},
  {"matmul_i8s",(ScicosF) matmul_i8s},
  {"matmul_m",(ScicosF) matmul_m},
  {"matmul_ui16e",(ScicosF) matmul_ui16e},
  {"matmul_ui16n",(ScicosF) matmul_ui16n},
  {"matmul_ui16s",(ScicosF) matmul_ui16s},
  {"matmul_ui32e",(ScicosF) matmul_ui32e},
  {"matmul_ui32n",(ScicosF) matmul_ui32n},
  {"matmul_ui32s",(ScicosF) matmul_ui32s},
  {"matmul_ui8e",(ScicosF) matmul_ui8e},
  {"matmul_ui8n",(ScicosF) matmul_ui8n},
  {"matmul_ui8s",(ScicosF) matmul_ui8s},
  {"mattran_m",(ScicosF) mattran_m},
  {"matz_abs",(ScicosF) matz_abs},
  {"matz_absc",(ScicosF) matz_absc},
  {"matz_bksl",(ScicosF) matz_bksl},
  {"matz_cath",(ScicosF) matz_cath},
  {"matz_catv",(ScicosF) matz_catv},
  {"matz_conj",(ScicosF) matz_conj},
  {"matz_det",(ScicosF) matz_det},
  {"matz_diag",(ScicosF) matz_diag},
  {"matz_div",(ScicosF) matz_div},
  {"matz_expm",(ScicosF) matz_expm},
  {"matz_inv",(ScicosF) matz_inv},
  {"matz_lu",(ScicosF) matz_lu},
  {"matz_pinv",(ScicosF) matz_pinv},
  {"matz_reim",(ScicosF) matz_reim},
  {"matz_reimc",(ScicosF) matz_reimc},
  {"matz_reshape",(ScicosF) matz_reshape},
  {"matz_sing",(ScicosF) matz_sing},
  {"matz_sqrt",(ScicosF) matz_sqrt},
  {"matz_sum",(ScicosF) matz_sum},
  {"matz_sumc",(ScicosF) matz_sumc},
  {"matz_suml",(ScicosF) matz_suml},
  {"matz_svd",(ScicosF) matz_svd},
  {"matz_vps",(ScicosF) matz_vps},
  {"matz_vpv",(ScicosF) matz_vpv},
  {"matzmul2_m",(ScicosF) matzmul2_m},
  {"matzmul_m",(ScicosF) matzmul_m},
  {"matztran_m",(ScicosF) matztran_m},
  /*  {"maxblk",(ScicosF) F2C(maxblk)}, */
  {"maxblk",(ScicosF) scicos_max_block},
  /*  {"memo",(ScicosF) F2C(memo)}, */
  {"memo",(ScicosF) scicos_memo_block},
  /*  {"mfclck",(ScicosF) F2C(mfclck)}, */
  {"mfclck",(ScicosF) scicos_mfclck_block},
  /*  {"minblk",(ScicosF) F2C(minblk)}, */
  {"minblk",(ScicosF) scicos_min_block},
  /* {"minmax",(ScicosF) minmax}, */
  {"minmax",(ScicosF) scicos_minmax_block},
  /* {"modulo_count",(ScicosF) modulo_count}, */
  {"modulo_count",(ScicosF) scicos_modulo_count_block},
  {"mscope",(ScicosF) scicos_mscope_block},
  /* {"mswitch",(ScicosF) mswitch}, */
  {"mswitch",(ScicosF) scicos_mswitch_block},
  /* {"multiplex",(ScicosF) multiplex}, */
  {"multiplex",(ScicosF) scicos_multiplex_block},
  /*  {"mux",(ScicosF) F2C(mux)}, */
  {"mux",(ScicosF) scicos_mux_block},
  {"mvswitch",(ScicosF) scicos_mvswitch_block},
  /*  {"pload",(ScicosF) F2C(pload)}, */
  {"pload",(ScicosF) scicos_pload_block},
  /* {"plusblk",(ScicosF) plusblk}, */
  {"plusblk",(ScicosF) scicos_plus_block},
  /*  {"powblk",(ScicosF) F2C(powblk)}, */
  {"powblk",(ScicosF) scicos_pow_block},
  /* {"prod",(ScicosF) prod}, */
  {"prod",(ScicosF) scicos_prod_block},
  /* {"product",(ScicosF) product}, */
  {"product",(ScicosF) scicos_product_block},
  /*  {"qzcel",(ScicosF) F2C(qzcel)}, */
  {"qzcel",(ScicosF) scicos_qzcel_block},
  /*  {"qzflr",(ScicosF) F2C(qzflr)}, */
  {"qzflr",(ScicosF) scicos_qzflr_block},
  /*  {"qzrnd",(ScicosF) F2C(qzrnd)}, */
  {"qzrnd",(ScicosF) scicos_qzrnd_block},
  /*  {"qztrn",(ScicosF) F2C(qztrn)}, */
  {"qztrn",(ScicosF) scicos_qztrn_block},
  /* {"ramp",(ScicosF) ramp}, */
  {"ramp",(ScicosF) scicos_ramp_block},
  /* {"ratelimiter",(ScicosF) ratelimiter}, */
  {"ratelimiter",(ScicosF) scicos_ratelimiter_block},
  /* {"readau",(ScicosF) readau}, */
  {"readau",(ScicosF) scicos_readau_block},
  /* {"readc",(ScicosF) readc}, */
  {"readc",(ScicosF) scicos_readc_block},
  /*  {"readf",(ScicosF) F2C(readf)}, */
  {"readf",(ScicosF) scicos_readf_block},
  {"relational_op",(ScicosF) relational_op},
  {"relational_op_i16",(ScicosF) relational_op_i16},
  {"relational_op_i32",(ScicosF) relational_op_i32},
  {"relational_op_i8",(ScicosF) relational_op_i8},
  {"relational_op_ui16",(ScicosF) relational_op_ui16},
  {"relational_op_ui32",(ScicosF) relational_op_ui32},
  {"relational_op_ui8",(ScicosF) relational_op_ui8},
  /* {"relationalop",(ScicosF) relationalop}, */
  {"relationalop",(ScicosF) scicos_relationalop_block},
  /* {"relay",(ScicosF) relay}, */
  {"relay",(ScicosF) scicos_relay_block},
  {"ricc_m",(ScicosF) ricc_m},
  /*  {"rndblk",(ScicosF) F2C(rndblk)}, */
  {"rndblk",(ScicosF) scicos_rnd_block},
  {"rndblk_m", (ScicosF) rndblk_m},
  {"rndblk_m",(ScicosF) rndblk_m},
  {"rndblkz_m",(ScicosF) rndblkz_m},
  {"root_coef",(ScicosF) root_coef},
  {"rootz_coef",(ScicosF) rootz_coef},
  /*  {"samphold",(ScicosF) F2C(samphold)}, */
  {"samphold",(ScicosF) scicos_samphold_block},
  {"samphold4",(ScicosF) samphold4},
  {"samphold4_m",(ScicosF) samphold4_m},
  /* {"satur",(ScicosF) satur}, */
  {"satur",(ScicosF) scicos_satur_block},
  /*  {"sawtth",(ScicosF) F2C(sawtth)}, */
  {"sawtth",(ScicosF) scicos_sawtth_block},
  /* {"scalar2vector",(ScicosF) scalar2vector}, */
  {"scalar2vector",(ScicosF) scicos_scalar2vector_block},
  {"scicosexit",(ScicosF) scicosexit},
  {"scope",(ScicosF) scicos_scope_block},
  {"scopxy",(ScicosF) scicos_scopxy_block},
  {"scoxy",(ScicosF) scicos_scoxy_block},
  /*  {"selblk",(ScicosF) F2C(selblk)}, */
  {"selblk",(ScicosF) scicos_sel_block},
  {"selector",(ScicosF) scicos_selector_block},
  /* {"selector",(ScicosF) selector}, */
  {"selector_m",(ScicosF) selector_m},
  {"shift_16_LA",(ScicosF) shift_16_LA},
  {"shift_16_LC",(ScicosF) shift_16_LC},
  {"shift_16_RA",(ScicosF) shift_16_RA},
  {"shift_16_RC",(ScicosF) shift_16_RC},
  {"shift_32_LA",(ScicosF) shift_32_LA},
  {"shift_32_LC",(ScicosF) shift_32_LC},
  {"shift_32_RA",(ScicosF) shift_32_RA},
  {"shift_32_RC",(ScicosF) shift_32_RC},
  {"shift_8_LA",(ScicosF) shift_8_LA},
  {"shift_8_LC",(ScicosF) shift_8_LC},
  {"shift_8_RA",(ScicosF) shift_8_RA},
  {"shift_8_RC",(ScicosF) shift_8_RC},
  {"shift_u16_RA",(ScicosF) shift_u16_RA},
  {"shift_u32_RA",(ScicosF) shift_u32_RA},
  {"shift_u8_RA",(ScicosF) shift_u8_RA},
  {"signum",(ScicosF) scicos_signum_block},
  /* {"signum",(ScicosF) signum}, */
  {"sin_blk",(ScicosF) scicos_sin_block},
  /* {"sin_blk",(ScicosF) sin_blk}, */
  /*  {"sinblk",(ScicosF) F2C(sinblk)}, */
  {"sinblk",(ScicosF) scicos_sinblk_block},
  {"sinh_blk",(ScicosF) scicos_sinh_block},
  /* {"sinh_blk",(ScicosF) sinh_blk}, */
  {"slider",(ScicosF) scicos_slider_block},
  /* {"slider",(ScicosF) slider}, */
  /*  {"sqrblk",(ScicosF) F2C(sqrblk)}, */
  {"sqrblk",(ScicosF) scicos_sqr_block},
  {"step_func",(ScicosF) scicos_step_func_block},
  /* {"step_func",(ScicosF) step_func}, */
  {"submat",(ScicosF) submat},
  {"submatz",(ScicosF) submatz},
  {"sum",(ScicosF) scicos_sum_block},
  /* {"sum",(ScicosF) sum}, */
  /*  {"sum2",(ScicosF) F2C(sum2)}, */
  {"sum2",(ScicosF) scicos_sum2_block},
  /*  {"sum3",(ScicosF) F2C(sum3)}, */
  {"sum3",(ScicosF) scicos_sum3_block},
  {"summation",(ScicosF) scicos_summation_block},
  /* {"summation",(ScicosF) summation}, */
  {"summation_i16e",(ScicosF) summation_i16e},
  {"summation_i16n",(ScicosF) summation_i16n},
  {"summation_i16s",(ScicosF) summation_i16s},
  {"summation_i32e",(ScicosF) summation_i32e},
  {"summation_i32n",(ScicosF) summation_i32n},
  {"summation_i32s",(ScicosF) summation_i32s},
  {"summation_i8e",(ScicosF) summation_i8e},
  {"summation_i8n",(ScicosF) summation_i8n},
  {"summation_i8s",(ScicosF) summation_i8s},
  {"summation_ui16e",(ScicosF) summation_ui16e},
  {"summation_ui16n",(ScicosF) summation_ui16n},
  {"summation_ui16s",(ScicosF) summation_ui16s},
  {"summation_ui32e",(ScicosF) summation_ui32e},
  {"summation_ui32n",(ScicosF) summation_ui32n},
  {"summation_ui32s",(ScicosF) summation_ui32s},
  {"summation_ui8e",(ScicosF) summation_ui8e},
  {"summation_ui8n",(ScicosF) summation_ui8n},
  {"summation_ui8s",(ScicosF) summation_ui8s},
  {"summation_z",(ScicosF) summation_z},
  {"switch2",(ScicosF) scicos_switch2_block},
  /* {"switch2",(ScicosF) switch2}, */
  {"switch2_m",(ScicosF) switch2_m},
  {"switchn",(ScicosF) scicos_switchn_block},
  /* {"switchn",(ScicosF) switchn}, */
  {"tablex2d_c",(ScicosF) tablex2d_c},
  {"tan_blk",(ScicosF) scicos_tan_block},
  /* {"tan_blk",(ScicosF) tan_blk}, */
  /*  {"tanblk",(ScicosF) F2C(tanblk)}, */
  {"tanblk",(ScicosF) scicos_tanblk_block},
  {"tanh_blk",(ScicosF) scicos_tanh_block},
  /* {"tanh_blk",(ScicosF) tanh_blk}, */
  /*  {"tcslti",(ScicosF) F2C(tcslti)}, */
  {"tcslti",(ScicosF) scicos_tcslti_block},
  {"tcslti4",(ScicosF) tcslti4},
  /*  {"tcsltj",(ScicosF) F2C(tcsltj)}, */
  {"tcsltj",(ScicosF) scicos_tcsltj_block},
  {"tcsltj4",(ScicosF) tcsltj4},
  /*  {"timblk",(ScicosF) F2C(timblk)}, */
  {"timblk",(ScicosF) scicos_timblk_block},
  {"time_delay",(ScicosF) scicos_time_delay_block},
  /* {"time_delay",(ScicosF) time_delay}, */
  {"tows_c",(ScicosF) tows_c},
  {"transmit_or_zero",(ScicosF) scicos_transmit_or_zero_block},
  /*  {"trash",(ScicosF) F2C(trash)}, */
  {"trash",(ScicosF) scicos_trash_block},
  {"variable_delay",(ScicosF) scicos_variable_delay_block},
  /* {"variable_delay",(ScicosF) variable_delay}, */
  {"whileiterator",(ScicosF) whileiterator},
  {"writeau",(ScicosF) scicos_writeau_block},
  /* {"writeau",(ScicosF) writeau}, */
  {"writec",(ScicosF) scicos_writec_block},
  /* {"writec",(ScicosF) writec}, */
  /*  {"writef",(ScicosF) F2C(writef)}, */
  {"writef",(ScicosF) scicos_writef_block},
  /*  {"zcross",(ScicosF) F2C(zcross)}, */
  {"zcross",(ScicosF) scicos_zcross_block} ,
  {"zcross2",(ScicosF) scicos_zcross2_block},  
  /* {"zcross2",(ScicosF) zcross2}, */
  {NULL , (ScicosF) 0}
};

#endif
#endif 

