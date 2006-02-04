#ifndef __SCICOS_BLOCKS__ 
#define __SCICOS_BLOCKS__ 

/* 
 * block prototypes and block table 
 */

#define scicos_args_base  int *flag__, int *nevprt, double *t, double *xd, double *x, \
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,\
	       double *rpar, int *nrpar, int *ipar, int *nipar 

/* flag  nclock t    xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar  intabl  ni  outabl no */
#define scicos_args_F0  scicos_args_base, double *u, int *nu, double *y, int *ny 

/* flag  nclock t    xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar  u1 n1 u2 n2 .... */
#define scicos_args_F  scicos_args_base, double *uy1, int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3, \
	    double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,\
	    int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8, \
	    double *uy9, int *nuy9, double *uy10, int *nuy10, double *uy11, int *nuy11,double *uy12, int *nuy12, \
 	    double *uy13, int *nuy13, double *uy14, int *nuy14, double *uy15, int *nuy15,double *uy16, int *nuy16, \
            double *uy17, int *nuy17, double *uy18, int *nuy18

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar    args_in sz_in, n_in  args_out sz_out, n_out  */
#define scicos_args_F2 scicos_args_base, double **inptr, int *insz, int *nin, double **outptr, int *outsz, int *nout

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar   args_in sz_in, n_in  args_out sz_out, n_out g ng */
#define scicos_args_F2z scicos_args_F2 , double *g , int *ng

/* flag  nclock t residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar */
#define scicos_args_base_i  int *flag__, int *nevprt, double *t,double *res, double *xd, double *x, \
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,\
	       double *rpar, int *nrpar, int *ipar, int *nipar 

/* flag  nclock t residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar u1 nu1 u2 nu2 .... */
#define scicos_args_Fi scicos_args_base_i,  double *uy1, int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3, \
	    double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,\
	    int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8, \
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
typedef void (*ScicosF4) (scicos_block *,int );

typedef struct _scicos_block_table scicos_block_table ;

struct _scicos_block_table  {
  char *name;
  ScicosF fonc;
};

extern int scicos_affich_block(scicos_args_F0);
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

#endif 

/* this is to be included only once in scicos.c */

#ifdef TABSIM 

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
  {"cstblk",(ScicosF) scicos_cst_block},
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
  {"trash",(ScicosF) scicos_trash_block},
  {"variable_delay",(ScicosF) scicos_variable_delay_block},
  {"writeau",(ScicosF) scicos_writeau_block},
  {"writec",(ScicosF) scicos_writec_block},
  {"writef",(ScicosF) scicos_writef_block},
  {"zcross2",(ScicosF) scicos_zcross2_block},  
  {"zcross",(ScicosF) scicos_zcross_block} ,
  {NULL , (ScicosF) 0}
};
#endif 


